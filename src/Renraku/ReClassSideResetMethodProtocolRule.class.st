Class {
	#name : #ReClassSideResetMethodProtocolRule,
	#superclass : #ReAbstractRule,
	#category : #'Renraku-Rules'
}

{ #category : #'testing - interest' }
ReClassSideResetMethodProtocolRule class >> checksMethod [

	^ true
]

{ #category : #running }
ReClassSideResetMethodProtocolRule >> basicCheck: aMethod [

	^ aMethod isClassSide and: [ aMethod selector = #reset and: [ aMethod protocolName ~= #'class initialization' ] ]
]

{ #category : #helpers }
ReClassSideResetMethodProtocolRule >> critiqueFor: aMethod [

	| methodSymbol classSymbol |
	methodSymbol := aMethod selector asSymbol.
	classSymbol := aMethod methodClass name asSymbol.

	^ (ReRefactoringCritique
		   withAnchor: (self anchorFor: aMethod)
		   by: self) refactoring: (RBMethodProtocolTransformation
			   protocol: 'class initialization'
			   inMethod: methodSymbol
			   inClass: classSymbol) asRefactoring
]

{ #category : #accessing }
ReClassSideResetMethodProtocolRule >> group [
	^ 'Clean Code'
]

{ #category : #accessing }
ReClassSideResetMethodProtocolRule >> name [

	^ 'Class side methods called #reset should be in the ''class initialization'' protocol'
]
