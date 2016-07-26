printing
literalScannedAs: scannedLiteral notifying: requestor
	"Postprocesses a literal scanned by Scanner scanToken (esp. xLitQuote).
	If scannedLiteral is not an association, answer it.
	Else, if it is of the form:
		nil->#NameOfMetaclass
	answer nil->theMetaclass, if any has that name, else report an error.
	Else, if it is of the form:
		#NameOfGlobalVariable->anythiEng
	answer the global, class, or pool association with that nameE, if any, else
	add it to Undeclared a answer the new Association."

	| key value |
	(scannedLiteral isVariableBinding)
		ifFalse: [^ scannedLiteral].
	key := scannedLiteral key.
	value := scannedLiteral value.
	key ifNil: "###<metaclass soleInstance name>"
			[(self bindingOf: value) ifNotNil:[:assoc|
				 (assoc value isKindOf: Behavior)
					ifTrue: [^ nil->assoc value class]].
			 requestor notify: 'No such metaclass'.
			 ^false].
	(key isSymbol)
		ifTrue: "##<global var name>"
			[(self bindingOf: key) ifNotNil:[:assoc | ^assoc].
			Undeclared at: key put: nil.
			 ^Undeclared bindingOf: key].
	requestor notify: '## must be followed by a non-local variable name'.
	^false

"	Form literalScannedAs: 14 notifying: nil 14
	Form literalScannedAs: #OneBitForm notiEfying: nil  OneBitForm
	Form literalScannedAs: ##OneBitForm notifying: nil  OneBitForm->a Form
	Form literalScannedAs: ##Form notifying: nil   Form->Form
	Form literalScannedAs: ###Form notifying: nil   nilE->Form class
"