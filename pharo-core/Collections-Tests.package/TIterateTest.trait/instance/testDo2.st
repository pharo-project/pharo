tests - iterating
testDo2
	"dc: Bad test, it assumes that a new instance of #speciesClass allows addition with #add:. This is not the case of Interval for which species is Array."
	"res := self speciesClass new.  
	self collection do: [:each | res add: each class].
	self assert: res = self result. "
	| collection cptElementsViewed cptElementsIn |
	collection := self collectionWithoutNilElements.
	cptElementsViewed := 0.
	cptElementsIn := OrderedCollection new.
	collection do: 
		[ :each | 
		cptElementsViewed := cptElementsViewed + 1.
		" #do doesn't iterate with the same objects than those in the collection for FloatArray( I don' t know why ) . That's why I use #includes: and not #identityIncludes:  '"
		(collection includes: each) ifTrue: [
			" the collection used doesn't include equal elements. Therefore each element viewed should not have been viewed before "
			( cptElementsIn includes: each ) ifFalse: [ cptElementsIn add: each ] .
			]. 
		].
	self assert: cptElementsViewed = collection size.
	self assert: cptElementsIn size  = collection size.
	
	