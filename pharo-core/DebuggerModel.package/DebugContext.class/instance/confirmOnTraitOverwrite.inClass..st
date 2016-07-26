private
confirmOnTraitOverwrite: aSelector inClass: aClass
	"test if method originates from a trait and let the user choose which one should be change"

	| target method |
	target := self selectedClass.
	method := aClass methodNamed: aSelector.
	method isFromTrait ifTrue: [ |trait|
		trait := method origin.
		target := UIManager default
			chooseFrom: {('Create copy in ' , aClass name). 
			('Compile Trait method in ' , trait name)}
			values: {aClass. trait }
			title: 'Where do you want to compile this trait method?' ].
	^ target