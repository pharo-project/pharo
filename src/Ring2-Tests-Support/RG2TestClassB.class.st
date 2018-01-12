"
A supporting behavior for Ring tests.
"
Class {
	#name : #RG2TestClassB,
	#superclass : #Object,
	#traits : 'RG2TestTraitA + (RG2TestTraitB - {#c})',
	#classTraits : 'RG2TestTraitA classTrait + RG2TestTraitB classTrait',
	#category : #'Ring2-Tests-Support'
}
