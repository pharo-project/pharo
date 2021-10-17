Class {
	#name : #ReClassSideResetMethodShouldBeInClassInitializationProtocolRule,
	#superclass : #ReAbstractRule,
	#category : #'Renraku-Rules'
}

{ #category : #'testing - interest' }
ReClassSideResetMethodShouldBeInClassInitializationProtocolRule class >> checksMethod [
	
	^ true
]

{ #category : #running }
ReClassSideResetMethodShouldBeInClassInitializationProtocolRule >> basicCheck: aMethod [
	"If you end here, you should most likely implement this in a subclass"
	
	^ aMethod selector = #reset and: [ aMethod protocol ~= #'class initialization' ]
]

{ #category : #accessing }
ReClassSideResetMethodShouldBeInClassInitializationProtocolRule >> name [

	^ 'Class side methods called #reset should be in the ''class initialization'' protocol'
]
