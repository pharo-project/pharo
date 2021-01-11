Class {
	#name : #TFUFFIFunctionCallTest,
	#superclass : #TFUFFITestCase,
	#category : #'ThreadedFFI-UFFI-Tests'
}

{ #category : #tests }
TFUFFIFunctionCallTest >> calloutWithManyArguments: arg1 and: arg2 and: arg3 and: arg4 and: arg5 and: arg6 and: arg7 and: arg8 and: arg9 and: arg10 [
	^ self
		ffiCall:
			#(int multipleArgumentCallout #(int arg1 , int arg2 , int arg3 , int arg4 , int arg5 , int arg6 , int arg7 , int arg8 , int arg9 , int arg10))
]

{ #category : #tests }
TFUFFIFunctionCallTest >> longCallout: seconds [
	
	^ self ffiCall: #(int longCallout(int seconds))
]

{ #category : #tests }
TFUFFIFunctionCallTest >> shortCallout [
	
	^ self ffiCall: #(int shortCallout())
]

{ #category : #tests }
TFUFFIFunctionCallTest >> testCalloutWithManyArguments [
	
	| result |
	result := self
			calloutWithManyArguments: 1
			and: 2
			and: 3
			and: 4
			and: 5
			and: 6
			and: 7
			and: 8
			and: 9
			and: 10.
	self assert: result equals: 55
]

{ #category : #tests }
TFUFFIFunctionCallTest >> testLongCalloutDoesNotBlockImage [
	
	"Tricky test.
	This tests checks that during a callout the image is not completely suspended.
	To do that, we do a 1 second long callout in a separate Pharo process.
	When that process starts executing (signals the calloutEnter semaphore), we wait for half a second to let the callback really start executing and then do something before the callback returns."
	| calloutEnter calloutExit orderOfFinishing |

	"If we are running in the same thread execution, it will block the interpreter. So the test is skipped."
	(self ffiLibrary = TFTestLibraryUsingSameThreadRunner)
		ifTrue: [ ^ self skip ]. 
	
	calloutEnter := Semaphore new.
	calloutExit := Semaphore new.
	orderOfFinishing := OrderedCollection new.	
	[
		calloutEnter signal. 	
		self longCallout: 1.
		orderOfFinishing add: #finishedCallout.
		calloutExit signal ] fork.
	calloutEnter wait.
	0.3 seconds wait.
	orderOfFinishing add: #waitingForCallout.
	calloutExit wait.
	
	self assert: orderOfFinishing first equals: #waitingForCallout
]

{ #category : #tests }
TFUFFIFunctionCallTest >> testShortCallout [
	
	| semaphore result |
	semaphore := Semaphore new.
	
	[ 	result := self shortCallout.
		semaphore signal ] fork.
	semaphore wait.
	
	self assert: result equals: 42
]
