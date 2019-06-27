"
I am EyeDateAndTimeInspector.
I am an EyeInspector.

I am a specialized Inspector for DateAndTime.
I show  all individual elements by name, as well as a UTC representation.
"
Class {
	#name : #SpEyeDateAndTimeInspector,
	#superclass : #SpEyeInspector,
	#category : #'Spec2-Inspector-Base'
}

{ #category : #list }
SpEyeDateAndTimeInspector >> addSpecialFields: elements [

	elements add: (SpDynamicEyeElement 
		host: self object 
		label: 'year' 
		value: [ self object year ]).
			
	elements add: (SpDynamicEyeElement 
		host: self object 
		label: 'month of year' 
		value: [ self object monthIndex ]).
			
	elements add: (SpDynamicEyeElement 
		host: self object 
		label: 'day of month' 
		value: [ self object dayOfMonth ]).
		
	elements add: (SpDynamicEyeElement 
		host: self object 
		label: 'full hours' 
		value: [ self object hours ]).
			
	elements add: (SpDynamicEyeElement 
		host: self object 
		label: 'full minutes' 
		value: [ self object minutes ]).
			
	elements add: (SpDynamicEyeElement 
		host: self object 
		label: 'full seconds' 
		value: [ self object seconds ]).
		
	elements add: (SpDynamicEyeElement 
		host: self object 
		label: 'utc' 
		value: [ self object asUTC ])
]
