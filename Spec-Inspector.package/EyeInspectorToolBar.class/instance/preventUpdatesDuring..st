private
preventUpdatesDuring: aBlock
	"Used to prevent back-firing when the toolbar is update from the outside"
	isUpdating ifTrue: [ 
		^ self ].
	isUpdating := true.
	aBlock ensure: [ isUpdating := false ]