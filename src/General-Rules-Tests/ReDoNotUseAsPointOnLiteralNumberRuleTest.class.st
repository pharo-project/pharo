"
I test `ReDoNotUseAsPointOnLiteralNumberRule`: it reports `asPoint` sent to a literal number and rewrites it into a point built with `@` (e.g. `33 asPoint` becomes `33 @ 33`), and it ignores any other receiver.
"
Class {
	#name : 'ReDoNotUseAsPointOnLiteralNumberRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Coding Idiom Violation',
	#package : 'General-Rules-Tests',
	#tag : 'Coding Idiom Violation'
}
