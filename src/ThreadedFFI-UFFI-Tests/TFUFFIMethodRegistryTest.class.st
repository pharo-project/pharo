Class {
	#name : 'TFUFFIMethodRegistryTest',
	#superclass : 'TFUFFITestCase',
	#category : 'ThreadedFFI-UFFI-Tests',
	#package : 'ThreadedFFI-UFFI-Tests'
}

{ #category : 'tests' }
TFUFFIMethodRegistryTest >> testRegistryDoesNotRestoreUninstalledMethods [

	"Call the method to force installation"
	| ffiMethod |
	TFUFFITestClassWithTrait uniqueInstance shortCallout.
	
	ffiMethod := TFUFFITestClassWithTrait >> #shortCallout.
	ffiMethod recompile.
	FFIMethodRegistry uniqueInstance resetMethod: ffiMethod.

	self assert: TFUFFITestClassWithTrait localMethods first isInstalled.
]
