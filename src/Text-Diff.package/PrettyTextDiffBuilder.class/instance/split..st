initialize
split: aString 
	| formatted trimmed |
	trimmed := aString asString trimBoth.
	trimmed isEmpty ifTrue: [ ^super split: '' ].
	formatted := [ sourceClass
				source: trimmed;
				class: sourceClass;
				format] on: Error do: [ :ex | trimmed ].
	^ super split: formatted