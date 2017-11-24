Class {
	#name : #GTSpotterExceptionMock4Filter,
	#superclass : #GTFilter,
	#category : #'GT-Tests-Spotter-Exceptions-Mocks'
}

{ #category : #public }
GTSpotterExceptionMock4Filter >> value: aSpotterContext [
	ArithmeticError signal: 'Error while filtering' " Exception while filtering "
]
