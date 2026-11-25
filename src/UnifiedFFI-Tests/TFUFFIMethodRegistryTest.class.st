Class {
	#name : 'TFUFFIMethodRegistryTest',
	#superclass : 'TFUFFITestCase',
	#category : 'UnifiedFFI-Tests-Tests',
	#package : 'UnifiedFFI-Tests',
	#tag : 'Tests'
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
