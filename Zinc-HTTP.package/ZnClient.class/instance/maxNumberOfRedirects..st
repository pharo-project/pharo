options
maxNumberOfRedirects: count
	"Set the maximum number of HTTP redirect that will be followed to count.
	Note that when the #followsRedirect option is false, 
	no redirects will ever be followed regardless of the value of this option.
	A ZnTooManyRedirects will be signalled when the count reaches zero."

	self optionAt: #maxNumberOfRedirects put: count