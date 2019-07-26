Extension { #name : #CharacterSetComplement }

{ #category : #'*GT-InspectorExtensions-Core' }
CharacterSetComplement >> gtInspectorItemsIn: composite [

	"The default set is huge, inspect the Absent list, which is typically a manageable size"

	^ composite fastList
		title: 'Absent';
		display: [ absent asArray ];
		beMultiple;
		format: [ :each | GTObjectPrinter asTruncatedTextFrom: each ];
		send: [ :result | 
			"withSmalltalkSearch;	
		showOnly: 50;
		helpMessage: 'Quick selection field. Given your INPUT, it executes: self select: [:each | INPUT ]'."
			result
				ifNil: [ nil ]
				ifNotNil: [ result size = 1
						ifTrue: [ result anyOne ]
						ifFalse: [ self species withAll: result ]
					]
			]
]
