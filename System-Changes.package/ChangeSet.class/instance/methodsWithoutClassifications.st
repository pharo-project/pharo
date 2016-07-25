testing
methodsWithoutClassifications
	"Return a collection representing methods in the receiver which have not been categorized"

	| slips notClassified |

	notClassified := {'as yet unclassified' asSymbol. #all}.
	slips := OrderedCollection new.
	self changedClasses do:
		[:aClass |
		(self methodChangesAtClass: aClass name) associationsDo: 
				[:mAssoc | | aSelector | (aClass includesSelector:  (aSelector := mAssoc key)) ifTrue:
						[(notClassified includes: (aClass organization categoryOfElement: aSelector))
								ifTrue: [slips add: aClass name , ' ' , aSelector]]]].
	^ slips

	"Smalltalk browseMessageList: (ChangeSet current methodsWithoutClassifications) name: 'unclassified methods'"