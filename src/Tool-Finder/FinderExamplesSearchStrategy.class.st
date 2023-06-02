Class {
	#name : #FinderExamplesSearchStrategy,
	#superclass : #FinderSearchStrategy,
	#category : #'Tool-Finder-Base'
}

{ #category : #accessing }
FinderExamplesSearchStrategy class >> strategyName [

	^ #Examples
]

{ #category : #'private - example' }
FinderExamplesSearchStrategy >> computeWithMethodFinder [
	"Compute the selectors for the single example of receiver and args, in the very top pane"

	| data result dataObjects |
	(finder searchingString includes: $.) ifFalse: [ ^ #(  ) ]. "delete trailing period. This should be fixed in the Parser!"
	data := finder searchingString trimRight: [ :char | char isSeparator or: [ char = $. ] ].

	[ dataObjects := Smalltalk compiler evaluate: '{' , data , '}' ]
		on: CodeError , RuntimeSyntaxError
		do: [ :e |
			self inform: 'Error: ' , e messageText.
			^ #(  ) ]. "#( data1 data2 result )"

	dataObjects size < 2 ifTrue: [
		self inform:
			'If you are giving an example of receiver, \args, and result, please put periods between the parts.\Otherwise just type one selector fragment' withCRs.
		^ #(  ) ].

	result := MethodFinder new findMethodsByExampleInput: dataObjects allButLast andExpectedResult: dataObjects last.
	result isEmpty ifTrue: [
		self inform: 'no single method'.
		^ #(  ) ].
	^ result
]

{ #category : #actions }
FinderExamplesSearchStrategy >> constructDictionary [

	| result listOfResults listOfSelectors |
	result := Dictionary new.
	listOfResults := self computeWithMethodFinder.
	listOfSelectors := listOfResults collect: [ :each | each selector ].
	finder packagesSelection classesDo: [ :class |
		class methodsDo: [ :method |
			| index |
			(index := listOfSelectors indexOf: method selector) = 0 ifFalse: [
				| key value |
				key := listOfResults at: index.
				value := method methodClass.
				(result includesKey: key)
					ifTrue: [ (result at: key) add: value ]
					ifFalse: [
						result at: key put: (OrderedCollection new
								 add: value;
								 yourself) ] ] ] ].
	finder resultDictionary: result
]

{ #category : #testing }
FinderExamplesSearchStrategy >> isExamplesSearch [

	^ true
]
