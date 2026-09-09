Extension { #name : 'Object' }

{ #category : '*Deprecation' }
Object >> backwardCompatible: anExplanationString on: date in: version transformWith: aRule [
	"Automatically transform the backward compatible calls to this method"

	BackwardCompatibility new
		context: thisContext sender;
		explanation: anExplanationString;
		date: date;
		version: version;
		rule: aRule;
		signal
]

{ #category : '*Deprecation' }
Object >> deprecated: anExplanationString [
	"Warn that the sending method has been deprecated."

	Deprecation new
		context: thisContext sender;
		explanation: anExplanationString;
		signal
]

{ #category : '*Deprecation' }
Object >> deprecated: anExplanationString on: date in: version [
	"Warn that the sending method has been deprecated."

	Deprecation new
		context: thisContext sender;
		explanation: anExplanationString;
		date: date;
		version: version;
		signal
]

{ #category : '*Deprecation' }
Object >> deprecated: anExplanationString 
	on: date 
	in: version 
	transformWith: aRule [
	"Automatically transform the deprecated calls."

	Deprecation new
		context: thisContext sender;
		explanation: anExplanationString;
		date: date;
		version: version;
		rule: aRule;
		signal
]

{ #category : '*Deprecation' }
Object >> deprecated: anExplanationString 
	on: date 
	in: version 
	transformWith: aRule 
	when: conditionBlock [
	"Automatically transform the deprecated call if it matches the condition."

	Deprecation new
		context: thisContext sender;
		explanation: anExplanationString;
		date: date;
		version: version;
		rule: aRule;
		condition: conditionBlock;
		signal
]

{ #category : '*Deprecation' }
Object >> deprecated: anExplanationString transformWith: aRule [
	"Automatically transform the deprecated call."

	Deprecation new
		context: thisContext sender;
		explanation: anExplanationString;
		rule: aRule;
		signal
]

{ #category : '*Deprecation' }
Object >> deprecated: anExplanationString transformWith: aRule when: conditionBlock [
	"Automatically transform the deprecated call if it matches the condition."

	Deprecation new
		context: thisContext sender;
		explanation: anExplanationString;
		rule: aRule;
		condition: conditionBlock;
		signal
]
