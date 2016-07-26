moving changes
methodsWithoutComments
	"Return a collection representing methods in the receiver which have no precode comments"

	| slips |
	slips := OrderedCollection new.
	self changedClasses do:
		[:aClass |
		(self methodChangesAtClass: aClass name) associationsDo: 
				[:mAssoc | (#(remove addedThenRemoved) includes: mAssoc value) ifFalse:
					[(aClass includesSelector:  mAssoc key) ifTrue:
						[(aClass firstPrecodeCommentFor: mAssoc key) isEmptyOrNil
								ifTrue: [slips add: aClass name , ' ' , mAssoc key]]]]].
	^ slips

	"Smalltalk browseMessageList: (ChangeSet current methodsWithoutComments) name: 'methods lacking comments'"