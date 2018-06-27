Class {
	#name : #NewMethodFinderTest,
	#superclass : #TestCase,
	#category : #NewMethodFinder
}

{ #category : #tests }
NewMethodFinderTest >> testExceptionDoesNotStopTheFlow [

	| bottom res |
	bottom := NewMethodFinderBottomClass new.
	res := (NewMethodFinder new permutationGuess: { bottom . 1 } and: 1).
	self assert: (res includes: #oneArgumentMethod:).
]

{ #category : #tests }
NewMethodFinderTest >> testMethodApprovedInClassButDisapprovedInSuperClass [
	"If a method f is approved by a class A but disapproved by one of 
	 its superclass, its should be approved for A instance."
	
	|bottom result |
	bottom := NewMethodFinderBottomClass new.
	result := (NewMethodFinder new permutationGuess: { bottom } and: bottom).
	self assert: (result includes: #topDisapprovedButBottomApprovedMethod).
]

{ #category : #tests }
NewMethodFinderTest >> testMethodDisapprovedInClassButApprovedInSuperClass [
	"If a method f is disapproved by a class A, it should be disapproved 
	by any instance of A, no matter if A superclass approved it."
	
	|bottom result |
	bottom := NewMethodFinderBottomClass new.
	result := (NewMethodFinder new permutationGuess: { bottom } and: bottom).
	self assert: (result includes: #topApprovedButBottomDisapprovedMethod) equals: false.
]

{ #category : #tests }
NewMethodFinderTest >> testMethodNotDefinedAndDisapprovedInClassButApprovedInSuperClass [
	"If the superclass B of a class defines a methods f which is not
	 redefined in A, then any instance of A should accept f unless
	 f is not approved by A.
	 Indeed let imagine this case :
	     f calls g which is approved in B but dangerous in A.
	     Then f is approved in B but dangerous in B.
	
	 TODO analysis : need to have a forbiden methods set or need that any
	                 methods which is approved by A is in its appovedSet 
	                 even if its only defined in one of its superclasses.
                    1 is more flexible and evolutive as the forbidden methods
                    set is only necesary in these unpracticable case (which
                    are really not frequent), the only advantage of the second
                    solution if that it offers to not calculate the set of 
                    approved mathod going through the superclasses tree.   
	"
	
	|bottom result |
	bottom := NewMethodFinderBottomClass new.
	result := (NewMethodFinder new permutationGuess: { bottom } and: bottom).
	self assert: (result includes: #topApprovedButBottomDisapprovedMethod) equals: false.
]

{ #category : #tests }
NewMethodFinderTest >> testMethodNotDefinedInClassAndDisapprovedInSuperClass [
	"If the superclass B of a class A defines a not approved method f 
	 which is not approved in A, then any instance of A should not accept f."
	
	|bottom result |
	bottom := NewMethodFinderBottomClass new.
	result := (NewMethodFinder new permutationGuess: { bottom } and: bottom).
	self assert: (result includes: #onlyTopDefinedDisapprovedMethod) equals: false.
]

{ #category : #tests }
NewMethodFinderTest >> testMethodNotDefinedInClassButApprovedInSuperClass [
	"If the superclass B of a class defines a methods f which is not
	 redefined in A and which is not forbidden in A, then any instance
	 of A should accept f."
	
	|bottom result |
	bottom := NewMethodFinderBottomClass new.
	result := (NewMethodFinder new permutationGuess: { bottom } and: bottom).
	self assert: (result includes: #onlyTopDefinedApprovedMethod).
]

{ #category : #tests }
NewMethodFinderTest >> testMethodOnlyBottomDefinedAndApprovedInBottom [
	"If a method f is approved by a class A, then it
	 should be approved by any instance of A"
	
	|bottom result |
	bottom := NewMethodFinderBottomClass new.
	result := NewMethodFinder new permutationGuess: { bottom } and: bottom.
	self assert: (result includes: #onlyBottomDefinedApprovedMethod).
	
]

{ #category : #tests }
NewMethodFinderTest >> testMethodOnlyTopDefinedAndApprovedInTop [
	"If a method f is approved by a class A and is not redefined 
	 in a subclass B, then if there is no information on it in B,
	 it should be approved by any instance of B."
	
	|bottom result |
	bottom := NewMethodFinderBottomClass new.
	result := (NewMethodFinder new permutationGuess: { bottom } and: bottom).
	self assert: (result includes: #onlyTopDefinedApprovedMethod).
	
]

{ #category : #tests }
NewMethodFinderTest >> testPermutation [

	"Any order of the args should result in the same responding methods. Thus,
	 `-` should correspond to `5 . 4 -> 1` but also to `4 . 5 -> 1`.
	
	TODO : The result array should be sorted. The method which matches when 
	the arguments are not shuffled is more important than the one where the
	arguments order has to be changed."
	
	
	| bottom res args|
	bottom := NewMethodFinderBottomClass new.
	args := { bottom . 1 . 2 }.
	res := (NewMethodFinder new permutationGuess: args and: 1).
	args permutationsDo: [ :p |
		self assert: res equals: (NewMethodFinder new permutationGuess: p and: 1).
	].
]

{ #category : #tests }
NewMethodFinderTest >> testSideEffects [
	"The arguments that we tested should not be changed even if
	 a matching method modifies them"
	
	| bottom res |
	bottom := NewMethodFinderBottomClass new.
	bottom modifyMethod: 1.
	res := (NewMethodFinder new permutationGuess: { bottom . 5 } and: 5).
	self assert: (res includes: #modifyMethod:).
	self assert: (bottom instanceVariable) equals: 1.
]
