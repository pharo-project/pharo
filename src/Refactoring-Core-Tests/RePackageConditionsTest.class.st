Class {
	#name : 'RePackageConditionsTest',
	#superclass : 'TestCase',
	#category : 'Refactoring-Core-Tests-Conditions',
	#package : 'Refactoring-Core-Tests',
	#tag : 'Conditions'
}

{ #category : 'tests' }
RePackageConditionsTest >> testNamesAreDifferent [
    | condition |
    condition := RePackageNamesAreDifferentCondition new
        packageName: #'PackageA';
        newName: #'PackageB';
        yourself.
    self assert: condition check.
    self assert: condition violators isEmpty.
]

{ #category : 'tests' }
RePackageConditionsTest >> testNamesAreSame [
    | condition |
    condition := RePackageNamesAreDifferentCondition new
        packageName: #'PackageA';
        newName: #'PackageA';
        yourself.
    self deny: condition check.
    self assert: condition violators isNotEmpty.
]

{ #category : 'tests' }
RePackageConditionsTest >> testSourcePackageDoesNotExist [
    | condition organizer |
    organizer := self packageOrganizer.
    condition := ReSourcePackageExistsCondition new
        packageName: #'PackageNotExist';
        packageOrganizer: organizer.
    self deny: condition check.
    self assert: condition violators isNotEmpty.
]

{ #category : 'tests' }
RePackageConditionsTest >> testSourcePackageExists [
    | condition organizer |
    organizer := self packageOrganizer.
    condition := ReSourcePackageExistsCondition new
        packageName: #'Refactoring-Core';
        packageOrganizer: organizer;
        yourself.
    self assert: condition check.
    self assert: condition violators isEmpty.
]

{ #category : 'tests' }
RePackageConditionsTest >> testTargetPackageDoesNotExist [
    | condition organizer |
    organizer := self packageOrganizer.
    condition := ReTargetPackageDoesNotExistCondition new
        newName: #'PackageNotExist';
        packageOrganizer: organizer;
        yourself.
    self assert: condition check.
    self assert: condition violators isEmpty.
]

{ #category : 'tests' }
RePackageConditionsTest >> testTargetPackageExists [
    | condition organizer |
    organizer := self packageOrganizer.
    condition := ReTargetPackageDoesNotExistCondition new
        newName: #'Refactoring-Core';
        packageOrganizer: organizer;
        yourself.
    self deny: condition check.
    self assert: condition violators isNotEmpty.
]
