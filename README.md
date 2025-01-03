## About North
North is a library that implements the OAuth 1.0a consumer and provider protocols. It allows you to connect to an OAuth provider and request its resources, or to build your own OAuth provider.

## How To: Client
If you want to connect to an OAuth provider, simply instantiate a `client` and pass it the required parameters. Following the Twitter requirements and using the tokens from [Chirp](http://shinmera.github.io/chirp/), we end up with this:

    (defvar *client*
      (make-instance
       'north:client
       :key "D1pMCK17gI10bQ6orBPS0w"
       :secret "BfkvKNRRMoBPkEtDYAAOPW4s2G9U8Z7u3KAf0dBUA"
       :request-token-uri "https://api.twitter.com/oauth/request_token"
       :authorize-uri "https://api.twitter.com/oauth/authorize"
       :access-token-uri "https://api.twitter.com/oauth/access_token"))
    
Next we start the authentication process:

    (north:initiate-authentication *client*)
    
Visit the returned URL and enter the verification code:

    (north:complete-authentication *client* ".....")
    
And finally we can access some resources:

    (ql:quickload :com.inuoe.jzon)
    (defmacro with-json-decoding (() &body body)
      `(let ((drakma:*text-content-types* (list* '("application" . "json") drakma:*text-content-types*)))
         (com.inuoe.jzon:parse
          (progn ,@body))))
    
    (with-json-decoding ()
      (north:make-signed-request *client* "https://api.twitter.com/1.1/account/verify_credentials.json" :get
                                 :params '(("include_entities" . "true"))))
    
    (with-json-decoding ()
      (north:make-signed-request *client* "https://api.twitter.com/1.1/statuses/update.json" :post
                                 :params '(("status" . "North and South, no matter where I look there's parens."))))

We can also post some data:

    (with-json-decoding ()
      (north:make-signed-data-request *client* "https://api.twitter.com/1.1/statuses/update_with_media.json"
                                      `(("media[]" . #p"~/sweet-bear.jpg"))
                                      :params '(("status" . "Check out this sweet bear!"))))

In order to keep the access token and secret so you can resume your session without repeatedly logging in every time, you can serialise the client with `make-load-form`.

## How To: Server
In order to provide a server you need to have a way of persisting two main pieces of information: applications, and sessions. The minimal amount of information necessary for an application is its key and secret. You most likely want to add additional information such as a name, icon, and description so that they can be displayed to the user when they authorise a consumer. A session needs to store quite a few pieces more: a token, token-secret, verifier token, callback, the application key, and the access rights. Default classes to contain all this information are provided through `application` and `session`.

However, the persistence and bookkeeping of these objects is still up to your server implementations. To do this, you should subclass `server` and implement methods for `make-application`, `make-session`, `application`, `session`, `rehash-session`, `revoke-application`, `revoke-session`, `record-nonce`, and `find-nonce`. What exactly these functions should accomplish is described in their docstrings and should be fairly obvious.

Once that is done, what's left to do is create a webservice with at least three endpoints. One for the request token, one for the authorization page, and one for the access token.

### General Behaviour for All Endpoints
For each endpoint there exists a corresponding function to call, which will return a number of values that should be returned to the user. You can use `alist->oauth-response` to construct a properly formatted response body. The objects that you need to pass to these calls are your `server` instance and a `request` instance that encapsulates the data that the server received with the request.

Each endpoint function performs certain checks against the request and in case of problems signals an error. Your server should intercept these and act as follows: if the error is a `parameter-error`, the HTTP response code should be `400`, and if the error is a `verification-error`, it should be `401`. Any other error is up to you. You may display information about the error in the response body, but the exact formatting thereof is up to you as well. It is however a good idea to output the same data format as your other endpoints would, aside from the oauth specific ones.

### The Request Token Endpoint
The request token endpoint should call `oauth/request-token` and return the values as `oauth_token`, `oauth_token_secret`, and `oauth_callback_confirmed` respectively.

### The Authorize Endpoint
The authorize page is special in the sense that it is not called with an oauth signed request. Instead the user calls it through a browser and the only thing it receives is the request token. The provider must then first authenticate the user and after having done so, display a page to the user that shows information about the application that they're connecting through and give them the option to either allow the application access or deny it. 

If the user selects allow, `oauth/authorize` should be called. If it returns a third value, the server should cause the user to be redirected to that URL. If not, the server should display a page that shows the second value, which is the verification token. The user must then copy this value into the consumer.

If the user selects deny, the provider must not necessarily do anything except ensure that the `session` is revoked and thus prevent the consumer from gaining access. It is not required to notify the consumer of this in any way.

### The Access Token Endpoint
The access token endpoint should call `oauth/access-token` and return the values as `oauth_token`, and `oauth_token_secret` respectively.

### Protected Resource Endpoint
Any such endpoint should call `oauth/verify`. This function returns nothing useful and only performs checks that if failed result in an error being signalled as usual. You may additionally want to check the session for permissions to access the specific endpoint if such a distinction exists. However, such additional functionality is up to you to design.

## An Example Server / Client Setup
See the [north-example](https://github.com/Shinmera/north/tree/master/example) system for a primitive, simple setup of a provider and consumer.

## OAuth Overview
OAuth is supposed to provide a relatively convenient standardised way to authenticate a user against a service (provider), and then allow the application (consumer) to access resources on the user's behalf. Key to this are two parts, the signing process, and the actual authorisation process itself.

### The Authorisation Process
Before an OAuth consumer can access any resources, it needs to obtain an access token. The path to this token happens in three distinct steps, during which several pieces of information need to be remembered and modified on both the consumer and provider's sides.

#### Step 0: Generating an Application
Unfortunately this is already where things begin to become awkward with the OAuth specification. The spec conflates what is essentially an application with the consumer. In order to improve clarity, North makes a distinction here. An application is a server-side instance that identifies a range of consumers. A consumer is a specific instance of a program that would like to connect to a provider through an application.

Whatever the case may be, before you can connect at all, you need to have access to an application key and secret. These two pieces are vital in the signing and requesting process. In North, this can be done through `make-application`. Once you have an application, you can get its `key` and `secret`.

#### Step 1: Requesting a Request Token
Now we actually start exchanging things with the provider as a consumer. To do this we send a signed request to the server's request-token endpoint. The request is signed using our application's secret, and we pass along the application's key as an OAuth parameter. Additionally we must give the provider a callback parameter that tells it what to do in the next step.

The provider then verifies our signature, and if it thinks everything is proper, then it sends back a body containing a request token and request secret. From here on out our requests will need to contain the token as an OAuth parameter and be signed with the request secret along with the application secret.

#### Step 2: The User Authorises the Consumer
The next step has to be done by the user themselves, such that granting access is always done through explicit consent. To do this, the consumer constructs an URL to the provider's authorize endpoint with the request token added as a GET parameter. The user then has to visit this URL.

Once on the page, the user should be confronted with a confirmation dialog, potentially also displaying all the things the consumer could get access to, if authorised. If the user accepts, the server generates a verifier token, which is then passed back to the consumer. This happens either automatically through a redirect, if the callback was a valid URL, or through the user manually, if the callback was the string "oob". In the latter case, the verifier is displayed to the user on the website, and they have to copy it into the consumer.

#### Step 3: Requesting an Access Token
The final step is to exchange the request token with an access token, using the verifier that was obtained in the previous section. To do this, a fully signed request is made to the provider's access-token endpoint, which verifies the request and verifier, rehashes the consumer's token, and upgrades its access. It then returns the newly generated access token and secret that will henceforth be used to sign requests.

#### Step 4: Accessing Resources
Now that we have an access token, we can start requesting whatever resources we were permitted to. A provider may for example offer several kinds of access tokens with varying ranges of permissions. The request endpoints are of course the provider's own decision. The only thing that OAuth specifies henceforth is that the request must be fully signed.

### Error Handling
Now, you may notice in all this that there's no specification on how to handle errors. And you're right. The only thing OAuth says is that missing/duplicate parameters must result in an HTTP return code of 400, and an invalid signature must result in a return code of 401. Everything else, what specifically went wrong, how to fix it, or any other thing that might be useful is unspecified. Some providers do give some error information, but the format in which the error is presented is up to them.

As such, North makes no assumptions whatsoever about how to parse the body contents on an error. You will, unfortunately, have to deal with that on your own. To do this, handle the `request-failed` error and parse the `body` of it.

### The Signing Process
Now comes the hard part. The actual signing process is unbelievably convoluted and incredibly easy to mess up. Let's start simple.

#### The Authorization Header
A signed request must contain an "Authorization" header, which must begin with the string "OAuth ". Following it are a series of oauth parameters. Each key/value pair is separated by a comma and a space, each key is separated from the value by an equals sign, and each value is surrounded by double-quotes. Each key and value must also be url-encoded. Keep in mind here that in order to transmit the request, this header is again url-encoded.

#### The OAuth Parameters
For each request a certain set of OAuth parameters must be present. Every request must have the `oauth_consumer_key`, `oauth_signature_method`, `oauth_signature`, `oauth_timestamp`, and `oauth_nonce`. Additional parameters are `oauth_callback`, `oauth_token`, and `oauth_verifier`, and whatever else the provider may want to require. These parameters must be ordered lexicographically by their keys and in case of duplicate keys by their values.

The `oauth_consumer_key` is the application key that we obtained in step 0. The `oauth_timestamp` must be an integer representing the unix-time when the request was formed. The `oauth_nonce` must be a unique string for each timestamp, but must not necessarily be globally unique.

#### The OAuth Signature
The final piece is the signature itself. The signature is created using a signing method (`oauth_signature_method`) over a signature base string, using a signature key.

The signature key is simply the application secret, appended with an &, appended with the token secret, if we have one.

The signature base string is constructed by the uppercase representation of the request method (POST/GET) followed by an &, followed by the url-encoded, normalised url, followed by an &, followed by the url-encoded, concatenated parameters. 

##### Normalising the URL
The URL must be normalised such that the schema is in all lowercase and the port is omitted if it is 80 and the schema is http, or if it is 443 and the schema is https.

##### Concatenating the Parameters
The parameters here are all of the parameters. Namely the oauth parameters (except the signature itself), the get parameters, and the post parameters. The parameters must again be sorted as before, but now they are concatenated differently. Namely each pair is separated by an ampersand, the key is separated from the value by an equals sign, and both keys and values must be url-encoded. Note that unlike before, values must not be surrounded by double-quotes.

##### Signing the Token
Potentially any signature method you might want is supported. The provider can request whatever they want. North implements the plaintext and hmac-sha1 methods suggested by the spec. See the relevant hashing methods for information on how they work.

### A Reminder, Just For Fun
The request is sent, url-encoded. The authorization header has url-encoded parameter values. One of those is the signature, which was constructed from a base string that has an url-encoded url part, and an url-encoded parameter part. The parameter part has url-encoded parameter values. Url-encode!
