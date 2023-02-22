Extension { #name : #SequenceableCollection }

{ #category : #'*Math-Operations-Extensions' }
SequenceableCollection >> closeTo: aSequenceableCollection [
	"Return true if all my elements are close to the elements of same index of the parameter"

	"(#(1.9283901234902349 2.302949083493849) closeTo: #(1.9283901234902348 2.302949083493899)) >>> true"

	^ self closeTo: aSequenceableCollection precision: Float defaultComparisonPrecision
]

{ #category : #'*Math-Operations-Extensions' }
SequenceableCollection >> closeTo: aSequenceableCollection precision: aPrecision [
	"Return true if all my elements are close to the elements of same index of the parameter with a certain precision"

	"(#(1.9283 2.3029) closeTo: #(1.9284 2.3028) precision: 0.001) >>> true"

	self with: aSequenceableCollection do: [ :a :b | (a closeTo: b precision: aPrecision) ifFalse: [ ^ false ] ].
	^ true
]
