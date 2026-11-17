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
Object >> deprecated: anExplanationString in: version [
	"Warn that the sending method has been deprecated in a Pharo version, such as #Pharo14. PHEP-0004: the version decides how long the method is kept."

	Deprecation new
		context: thisContext sender;
		explanation: anExplanationString;
		version: version;
		signal
]

{ #category : '*Deprecation' }
Object >> deprecated: anExplanationString in: version transformWith: aRule [
	"Warn that the sending method has been deprecated in a Pharo version, such as #Pharo14, and rewrite its callers with aRule at the end of its compatibility period (PHEP-0004)."

	Deprecation new
		context: thisContext sender;
		explanation: anExplanationString;
		version: version;
		rule: aRule;
		signal
]

{ #category : '*Deprecation' }
Object >> deprecated: anExplanationString in: version transformWith: aRule when: conditionBlock [
	"Warn that the sending method has been deprecated in a Pharo version, such as #Pharo14, and rewrite the callers that match conditionBlock with aRule at the end of its compatibility period (PHEP-0004)."

	Deprecation new
		context: thisContext sender;
		explanation: anExplanationString;
		version: version;
		rule: aRule;
		condition: conditionBlock;
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
