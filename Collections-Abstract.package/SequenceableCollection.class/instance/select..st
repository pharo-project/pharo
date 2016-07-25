enumerating
select: aBlock 
	"Optimized version of Collection>>#select:"
	
	| each |
	^ self class new: self size streamContents: [ :stream|
		1 to: self size do: [ :index |
			(aBlock value: (each := self at: index))
				ifTrue: [ stream nextPut: each ]]]