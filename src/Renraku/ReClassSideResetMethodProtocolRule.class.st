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
	"If you end here, you should most likely implement this in a subclass"
	
	^ aMethod selector = #reset and: [ aMethod protocol ~= #'class initialization' ]
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
ReClassSideResetMethodProtocolRule >> name [

	^ 'Class side methods called #reset should be in the ''class initialization'' protocol'
]
