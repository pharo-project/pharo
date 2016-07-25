options
maxNumberOfRedirects
	"Return the maximum number of HTTP redirect that will be followed.
	Note that when the #followsRedirect option is false, 
	no redirects will ever be followed regardless of the value of this option.
	Defaults to 3. A ZnTooManyRedirects will be signalled when the count reaches zero."

	^ self optionAt: #maxNumberOfRedirects ifAbsent: [ 3 ]