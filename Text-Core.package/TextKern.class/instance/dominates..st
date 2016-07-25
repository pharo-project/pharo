as yet unclassified
dominates: other
	"NOTE: The use of active in this code is specific to its use in the method
		Text class addAttribute: att toArray: others"
	(active and: [other class == self class and: [other kern + kern = 0]])
		ifTrue: [active := false.  ^ true].  "can only dominate once"
	^ false