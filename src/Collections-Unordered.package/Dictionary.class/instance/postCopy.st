copying
postCopy
	"Must copy the associations, or later store will affect both the
original and the copy"

	array := array collect: [ :association |
		association ifNotNil: [ association copy ] ]