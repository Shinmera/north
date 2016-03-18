#|
 This file is a part of north
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.north)

;; client.lisp
(docs:define-docs
  (function call-request
    "Executes the given request object.

If the http status code returned is 200, the response body is parsed into an alist
by OAUTH-RESPONSE->ALIST. Otherwise, an error of type REQUEST-FAILED is signalled.

See REQUEST
See OAUTH-RESPONSE->ALIST
See REQUEST-FAILED")

  (function call-signed
    "Execute the given request object after signing it.

See REQUEST
See CALL-REQUEST
See MAKE-AUTHORIZED
See MAKE-SIGNED")

  (function make-signed-request
    "Construct and execute a signed request for the given client.

Returns the result of the request execution as the first value and the constructed
request object itself as the second.

See CLIENT
See REQUEST
See CALL-SIGNED")

  (function initiate-authentication
    "Start the authentication process for the client.

This performs an oauth/request-token request and constructs the proper 
oauth/authorize URL for the user to visit. This address is returned.
If the provider did not confirm the callback, an error of type CALLBACK-UNCONFIRMED
is signalled.
This operation modifies the TOKEN and TOKEN-SECRET fields of the client.

See CLIENT
See MAKE-SIGNED-REQUEST")

  (function complete-authentication
    "Complete the authentication process for the client.

This performs an oauth/access-token request.
This operation modifies the TOKEN and TOKEN-SECRET fields of the client.
When the client has a VERIFY-URI given, an additional oauth/verify request is made
to test whether the full process was complete.

See CLIENT
See MAKE-SIGNED-REQUEST")

  (type client
    "An oAuth client class to encapsulate a single connection to a provider.

Contains all the necessary information and state to manage the connection.
You may serialise this object into a reloadable form using MAKE-LOAD-FORM.
Unless the provider expires access tokens or the token is revoked for some reason
or another, the authentication process for a client has to be done only once.

See KEY
See SECRET
See TOKEN
See TOKEN-SECRET
See CALLBACK
See REQUEST-TOKEN-URI
See AUTHORIZE-URI
See ACCESS-TOKEN-URI
See VERIFY-URI
See INITIATE-AUTHENTICATION
See COMPLETE-AUTHENTICATION")

  (function key
    "Accesses the application key.")

  (function secret
    "Accesses the application secret.")

  (function token
    "Accesses the current (access or request) token.")

  (function token-secret
    "Accesses the current (access or request) token secret.")

  (function callback
    "Accesses the callback to which the oauth/authorize step should redirect.
If it should not redirect, the callback must be exactly the string \"oob\".")

  (function request-token-uri
    "Accesses the oauth/request-token uri, the first endpoint for the oAuth process.")

  (function authorize-uri
    "Accesses the oauth/authorize uri, the second endpoint for the oAuth process.")

  (function access-token-uri
    "Accesses the oauth/access-token uri, the third endpoint for the oAuth process.")

  (function verify-uri
    "Accesses the oauth/verify uri, an optional endpoint to test whether the process completed successfully."))

;; conditions.lisp
(docs:define-docs
  (type north-condition
    "Base condition class for conditions in the North system.

See REQUEST")

  (function request
    "The request object that the error occurred on.

See NORTH-CONDITION")

  (type parameter-error
    "An error signalled when the parameters given to the provider are incomplete or badly specified.
Should end up in an HTTP 400 return code.

See NORTH-CONDITION")

  (type verification-error
    "An error signalled when the parameters given to the provider fail the verification test.
Should end up in an HTTP 401 return code.

See NORTH-CONDITION")

  (type client-error
    "An error signalled when the oAuth client encounters a problem.

See NORTH-CONDITION")

  (type parameters-missing
    "An error signalled when the oAuth request does not include all the required oAuth parameters.

See PARAMETERS
See PARAMETER-ERROR")

  (function parameters
    "Accessor for the list of missing parameters that should have been supplied, but weren't.

See PARAMETERS-MISSING")

  (type bad-version
    "An error signalled when the oAuth request specifies a bad version field.

See PARAMETER-ERROR")

  (type verifier-taken
    "An error signalled when a verifier token for a request is re-used or the authorization step is repeated for the same request token.

See VERIFICATION-ERROR")

  (type nonce-reused
    "An error signalled when a nonce is used twice within the same timestamp.

See VERIFICATION-ERROR")

  (type invalid-signature
    "An error signalled when the oAuth signature cannot be verified. 
This is most likely due to a bad signing procedure on the client's behalf, or a 
disagreement about the tokens and secrets used in the signing process.

See VERIFICATION-ERROR")

  (type invalid-verifier
    "An error signalled when the verifier of the request is invalid for the associated request token.

See VERIFICATION-ERROR")

  (type invalid-token
    "An error signalled when the token of the request is invalid or unknown.

See VERIFICATION-ERROR")

  (type invalid-application
    "An error signalled when the oauth_consumer_key of the request is invalid or unknown.

See VERIFICATION-ERROR")

  (type request-failed
    "An error signalled when a client's request returned with a non-200 status code.

See BODY
See STATUS-CODE
See HEADERS
See CLIENT-ERROR")

  (function body
    "The returned http body of the failed request. This may be an octet-vector if the content type is not known to drakma.

See REQUEST-FAILED
See DRAKMA:*TEXT-CONTENT-TYPES*")

  (function status-code
    "The returned status code of the failed request.

See REQUEST-FAILED")

  (function headers
    "The returned header of the failed request as an alist.

See REQUEST-FAILED")

  (type callback-unconfirmed
    "An error signalled when the provider returns a non-\"true\" value for the oauth_callback_confirmed key.

See CLIENT-ERROR"))

;; request.lisp
(docs:define-docs
  (function make-signed
    "Modifies the request to add a signature to the oauth parameters.

This will also modify the oauth parameters list to remove duplicates or empty values.
Returns the request.

See REQUEST
See CREATE-SIGNATURE")

  (function make-authorized
    "Modifies the request to add the authorization header using the oauth parameters.

If the oauth_signature parameter is missing, an error is signalled.
Returns the request.

See REQUEST")

  (function verify
    "Verifies whether the signature in the request is valid.
To do this it destructures the Authorization header and uses it values to construct a 
new signature. This is then compared against the oauth_signature value in the oauth
alist.

See REQUEST
See DESTRUCTURE-OAUTH-HEADER
See CREATE-SIGNATURE")

  (type request
    "Container class to represent an HTTP request.

Upon initialisation a few default oauth parameters are set, if not given already:
  oauth_nonce             Set to (make-nonce)
  oauth_signature_method  Set to \"HMAC-SHA1\"
  oauth_timestamp         Set to (make-timestamp)
  oauth_version           Set to \"1.0\"
Additionally, if the headers given include an Authorization header, then the
oauth parameters are overridden by the results of DESTRUCTURE-OAUTH-HEADER.

See HTTP-METHOD
See URL
See GET-PARAMS
See POST-PARAMS
See HEADERS
See OAUTH
See DESTRUCTURE-OAUTH-HEADER")
  
  (function http-method
    "Accesses the HTTP-METHOD (GET/POST) of the request.")

  (function url
    "Accesses the URL of the request.")

  (function get-params
    "Accesses the GET parameters of the request.")

  (function post-params
    "Accesses the POST parameters of the request.")

  (function headers
    "Accesses the HTTP headers of the request.")

  (function oauth
    "Accesses the pure oauth parameters of the request."))

;; server.lisp
(docs:define-docs
  (function make-application
    "Creates and adds a new application object to the server.

Additionally supported keyword arguments are used as initargs for the application
instance. Which application class is used depends on the server.

See SERVER
See APPLICATION")
  
  (function make-session
    "Creates and adds a new session object to the server.

Additionally supported keyword arguments are used as initargs for the session
instance. Which session class is used depends on the server.

See SERVER
See SESSION")
  
  (function application
    "Returns the application object associated with the given key on the server, if any.

See SERVER
See APPLICATION")
  
  (function session
    "Returns the session object associated with the given token on the server, if any.

See SERVER
See SESSION")
  
  (function rehash-session
    "Updates the session object to use a new token and token-secret.

The server must ensure that the session will no longer be accessible through SESSION
using the old token, but will be accessible through the newly generated token.

See SERVER
See SESSION")
  
  (function revoke-application
    "Removes the given application from the server.

The application must be no longer reachable through CONSUMER on the server and all
sessions authorized through this application must be invalidated.

See SERVER
See APPLICATION
See SESSION")
  
  (function revoke-session
    "Removes the given session from the server.

The session must no longer be valid and any client trying to use its tokens to
access any resource or perform any step of the oauth process must be rejected.

See SERVER
See SESSION")
  
  (function record-nonce
    "Remember the given nonce for the used timestamp.")
  
  (function find-nonce
    "If the given nonce was used before on the given timestamp, return non-NIL.")
  
  (function oauth/request-token
    "Perform the oauth/request-token step of the process.

This creates a new session object and returns three values:
  TOKEN               --- The newly generated request token.
  TOKEN-SECRET        --- The newly generated request token secret.
  CALLBACK-CONFIRMED  --- Whether the callback has been confirmed. According to
                          the spec, this should always be T.

Signals a PARAMETER-ERROR or VERIFICATION-ERROR on an invalid request.

See SERVER
See MAKE-SESSION
See TOKEN
See TOKEN-SECRET")
  
  (function oauth/authorize
    "Perform the oauth/authorize step of the process.

This verifies the request and returns three values:
  TOKEN     --- The current request token.
  VERIFIER  --- The (possibly newly generated) verifier for the next step.
  URL       --- The callback URL to redirect to if the callback is not \"oob\".
                If the callback is indeed \"oob\", NIL is returned for this.

Signals a VERIFICATION-ERROR on an invalid request.

See SERVER
See TOKEN
See VERIFIER
See CALLBACK")
  
  (function oauth/access-token
    "Perform the oauth/access-token step of the process.

This verifies the request and if successful, upgrades its access to full.
It will also invalidate the session's verifier and rehash it.

Returns two values:
  TOKEN         --- The newly generated access token.
  TOKEN-SECRET  --- The newly generated access token secret.

Signals a PARAMETER-ERROR or VERIFICATION-ERROR on an invalid request.

See SERVER
See VERIFIER
See REHASH-SESSION
See TOKEN
See TOKEN-SECRET")
  
  (function oauth/verify
    "Standard endpoint to use on any protected resource.

This verifies the request and makes sure it uses a valid access token.
Returns T on success.

Signals a PARAMETER-ERROR or VERIFICATION-ERROR on an invalid request."))

;; toolkit.lisp
(docs:define-docs
  (variable *external-format*
    "The external format to use to url-en/decode and execute requests.
Defaults to :UTF-8")

  (function pget
    "Easy accessor for alists. 

Checks keys STRING-EQUALly and returns the value directly, if found.
SETF-able. If a key is set that does not occur in the alist, a new
entry is PUSHed, otherwise the existing one is modified.")

  (function remove-param
    "Remove the given key from the alist.

Checks keys STRING-EQUALly and constructs a new alist.")

  (function url-encode
    "Encode the string into url-encoded format as required by the oAuth spec.

Namely all characters except [0-9], [a-z], [A-Z], and [-._~] are encoded.

See *EXTERNAL-FORMAT*")

  (function url-decode
    "Decode the string into plain text format.

See *EXTERNAL-FORMAT*")

  (function sign
    "Signs the given data using the specified method.

By default, :PLAINTEXT, :HMAC-SHA1, and :CMAC-AES are supported.
A string can be used for the method as well, but will be converted
to a keyword first.")

  (function make-nonce
    "Creates a default nonce by turning a V4 UUID into a string.")

  (function make-timestamp
    "Creates a default timestamp by turning a unix timestamp into a string.")

  (function param<
    "Compares two parameters as required by the oauth spec.

If the two keys of the parameters are equal, the values are compared instead.
Comparison happens by lexicographic string<.")

  (function sort-params
    "Creates a fresh list in which the parameters are sorted.

See PARAM<")

  (function concat-params
    "Concatenate the given alist into a single parameter string.

This intermits an equal sign between key and value, and DELIM between each pair.
If QUOTE is non-NIL, the values are surrounded by #\\\".")

  (function url-parts
    "Splits the given URL into its parts of SCHEME, HOST, PORT, and PATH.")

  (function normalize-url
    "Normalises the URL to avoid ambiguity. 

In specific, it downcases the scheme, removes leading slashes from the path, and
omits the 80 port if the scheme is HTTP, and the 443 port if the scheme is HTTPS.")

  (function normalize-token
    "Creates a normalized token as follows.

TOKEN   ::= METHOD '&' URL '&' PARAMS
METHOD  --- The method in uppercase
URL     --- The url-encoded and normalized URL.
PARAMS  --- The url-encoded and concatenated params.

See URL-ENCODE
See NORMALIZE-URL
See CONCAT-PARAMS")

  (function create-signature
    "Create an oAuth signature for the given parameters.

This does not include the oauth_signature parameters if it is passed in OAUTH-PARAMS.
Calls SIGN using the oauth_signature_method oauth param, a normalized token from the
OAUTH-PARAMS and GET-PARAMS, and the given CONSUMER-SECRET and TOKEN-SECRET.

See SIGN
See NORMALIZE-TOKEN")

  (function start-p
    "Returns T if START is found at the beginning of STRING.")

  (function destructure-oauth-header
    "Destructures an Authorization header into its oauth parameters.

Returns an alist of all the parameters with their associated values.
If the header is malformed, an error is signalled.")

  (function oauth-response->alist
    "Splits the body into an alist of keys and values.")

  (function alist->oauth-response
    "Concatenates an alist into an oauth response body.

See CONCAT-PARAMS."))
