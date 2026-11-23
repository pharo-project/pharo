"
I am a shared pool for the tests that serialize classes using a pool. I must stay a global: materializing a class that uses me looks me up by name.
"
Class {
	#name : 'FLTestSharedPool',
	#superclass : 'SharedPool',
	#category : 'Fuel-Core-Tests',
	#package : 'Fuel-Core-Tests'
}
