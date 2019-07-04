"
I am EyeDateAndTimeInspector.
I am an EyeInspector.

I am a specialized Inspector for DateAndTime.
I show  all individual elements by name, as well as a UTC representation.
"
Class {
	#name : #EyeDateAndTimeInspector,
	#superclass : #EyeInspector,
	#category : #'Spec2-Inspector-Base'
}

{ #category : #list }
EyeDateAndTimeInspector >> addSpecialFields: elements [

	elements add: (DynamicEyeElement 
		host: self object 
		label: 'year' 
		value: [ self object year ]).
			
	elements add: (DynamicEyeElement 
		host: self object 
		label: 'month of year' 
		value: [ self object monthIndex ]).
			
	elements add: (DynamicEyeElement 
		host: self object 
		label: 'day of month' 
		value: [ self object dayOfMonth ]).
		
	elements add: (DynamicEyeElement 
		host: self object 
		label: 'full hours' 
		value: [ self object hours ]).
			
	elements add: (DynamicEyeElement 
		host: self object 
		label: 'full minutes' 
		value: [ self object minutes ]).
			
	elements add: (DynamicEyeElement 
		host: self object 
		label: 'full seconds' 
		value: [ self object seconds ]).
		
	elements add: (DynamicEyeElement 
		host: self object 
		label: 'utc' 
		value: [ self object asUTC ])
]
