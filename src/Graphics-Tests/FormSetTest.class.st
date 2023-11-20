"
SUnit tests for class FormSet
"
Class {
	#name : 'FormSetTest',
	#superclass : 'ClassTestCase',
	#category : 'Graphics-Tests-Primitives',
	#package : 'Graphics-Tests',
	#tag : 'Primitives'
}

{ #category : 'asserting' }
FormSetTest >> assert: form isFilledWith: color [

	self assert:
		((0 to: form width - 1) allSatisfy: [ :x |
			(0 to: form height - 1) allSatisfy: [ :y |
				(form colorAt: x @ y) = color ] ])
]

{ #category : 'coverage' }
FormSetTest >> classToBeTested [

	^ FormSet
]

{ #category : 'tests - converting' }
FormSetTest >> testAsForm [

	| form1 form2 formSet result |

	(form1 := Form extent: 1@2 depth: 32)
		fillColor: Color red translucent.
	(form2 := Form extent: 2@4 depth: 32)
		fillColor: Color red translucent.

	formSet := FormSet form: form1.
	self assert: formSet asForm identicalTo: form1.

	formSet := FormSet forms: { form1. form2 }.
	self assert: formSet asForm identicalTo: form1.

	formSet := FormSet forms: { form2. form1 }.
	self assert: formSet asForm identicalTo: form2.

	formSet := FormSet extent: 2@4 depth: 32 forms: { form1. form2 }.
	self assert: formSet asForm identicalTo: form2.

	formSet := FormSet extent: 3@6 depth: 32 forms: { form1. form2 }.
	result := formSet asForm.
	self assert: result extent equals: 3@6.
	self assert: result depth equals: 32.
	self assert: result isFilledWith: Color red translucent.

	formSet := FormSet extent: 2@4 depth: 8 forms: { form1. form2 }.
	result := formSet asForm.
	self assert: result extent equals: 2@4.
	self assert: result depth equals: 8.
	self assert: result isFilledWith: Color red.
]

{ #category : 'tests - converting' }
FormSetTest >> testAsFormAtScale [

	| form1 form2 form3 formSet result |

	(form1 := Form extent: 1@2 depth: 32)
		fillColor: Color green translucent.
	(form2 := Form extent: 2@4 depth: 32)
		fillColor: Color green translucent.
	(form3 := Form extent: 4@8 depth: 32)
		fillColor: Color green translucent.	

	formSet := FormSet form: form1.
	self assert: (formSet asFormAtScale: 1) identicalTo: form1.

	formSet := FormSet forms: { form1. form2. form3 }.
	self assert: (formSet asFormAtScale: 1) identicalTo: form1.
	self assert: (formSet asFormAtScale: 2) identicalTo: form2.
	self assert: (formSet asFormAtScale: 4) identicalTo: form3.

	formSet := FormSet forms: { form2. form1. form3 }.
	self assert: (formSet asFormAtScale: 1) identicalTo: form2.
	self assert: (formSet asFormAtScale: 2) identicalTo: form3.

	formSet := FormSet extent: 2@4 depth: 32 forms: { form1. form2. form3 }.
	self assert: (formSet asFormAtScale: 1) identicalTo: form2.
	self assert: (formSet asFormAtScale: 2) identicalTo: form3.

	formSet := FormSet extent: 3@6 depth: 32 forms: { form1. form2. form3 }.
	result := formSet asFormAtScale: 1.
	self assert: result extent equals: 3@6.
	self assert: result depth equals: 32.
	self assert: result isFilledWith: Color green translucent.

	formSet := FormSet extent: 1@2 depth: 8 forms: { form1. form2. form3 }.
	result := formSet asFormAtScale: 1.
	self assert: result extent equals: 1@2.
	self assert: result depth equals: 8.
	self assert: result isFilledWith: Color green.
	result := formSet asFormAtScale: 2.
	self assert: result extent equals: 2@4.
	self assert: result depth equals: 8.
	self assert: result isFilledWith: Color green.
]

{ #category : 'tests - converting' }
FormSetTest >> testAsFormWithExtent [

	| form1 form2 form3 formSet result |

	(form1 := Form extent: 1@2 depth: 32)
		fillColor: Color blue translucent.
	(form2 := Form extent: 2@4 depth: 32)
		fillColor: Color blue translucent.
	(form3 := Form extent: 4@8 depth: 32)
		fillColor: Color blue translucent.

	formSet := FormSet form: form1.
	self assert: (formSet asFormWithExtent: 1@2) identicalTo: form1.
	result := formSet asFormWithExtent: 2@4.
	self assert: result extent equals: 2@4.
	self assert: result depth equals: 32.
	self assert: result isFilledWith: Color blue translucent.

	formSet := FormSet forms: { form2. form1. form3 }.
	self assert: (formSet asFormWithExtent: 1@2) identicalTo: form1.
	self assert: (formSet asFormWithExtent: 2@4) identicalTo: form2.
	self assert: (formSet asFormWithExtent: 4@8) identicalTo: form3.
	result := formSet asFormWithExtent: 3@6.
	self assert: result extent equals: 3@6.
	self assert: result depth equals: 32.
	self assert: result isFilledWith: Color blue translucent.

	formSet := FormSet extent: 2@4 depth: 32 forms: { form3. form2. form1 }.
	self assert: (formSet asFormWithExtent: 1@2) identicalTo: form1.
	self assert: (formSet asFormWithExtent: 2@4) identicalTo: form2.
	self assert: (formSet asFormWithExtent: 4@8) identicalTo: form3.
	result := formSet asFormWithExtent: 3@6.
	self assert: result extent equals: 3@6.
	self assert: result depth equals: 32.
	self assert: result isFilledWith: Color blue translucent.
	result := formSet asFormWithExtent: 4@5.
	self assert: result extent equals: 4@5.
	self assert: result depth equals: 32.
	self assert: result isFilledWith: Color blue translucent.

	formSet := FormSet extent: 1@2 depth: 8 forms: { form1. form2. form3 }.
	result := formSet asFormWithExtent: 1@2.
	self assert: result extent equals: 1@2.
	self assert: result depth equals: 8.
	self assert: result isFilledWith: Color blue.
]

{ #category : 'tests - accessing' }
FormSetTest >> testDepth [

	| form1 form2 formSet |

	form1 := Form extent: 1@2 depth: 32.
	form2 := Form extent: 2@4 depth: 8.

	formSet := FormSet form: form1.
	self assert: formSet depth equals: 32.

	formSet := FormSet form: form2.
	self assert: formSet depth equals: 8.

	formSet := FormSet forms: { form1. form2 }.
	self assert: formSet depth equals: 32.

	formSet := FormSet forms: { form2. form1 }.
	self assert: formSet depth equals: 8.

	formSet := FormSet extent: 1@2 depth: 32 forms: { form1. form2 }.
	self assert: formSet depth equals: 32.

	formSet := FormSet extent: 2@4 depth: 16 forms: { form2. form1 }.
	self assert: formSet depth equals: 16.
]

{ #category : 'tests - accessing' }
FormSetTest >> testExtent [

	| form1 form2 formSet |

	form1 := Form extent: 1@2 depth: 32.
	form2 := Form extent: 2@4 depth: 8.

	formSet := FormSet form: form1.
	self assert: formSet extent equals: 1@2.

	formSet := FormSet form: form2.
	self assert: formSet extent equals: 2@4.

	formSet := FormSet forms: { form1. form2 }.
	self assert: formSet extent equals: 1@2.

	formSet := FormSet forms: { form2. form1 }.
	self assert: formSet extent equals: 2@4.

	formSet := FormSet extent: 1@2 depth: 32 forms: { form1. form2 }.
	self assert: formSet extent equals: 1@2.

	formSet := FormSet extent: 3@4 depth: 32 forms: { form2. form1 }.
	self assert: formSet extent equals: 3@4.
]

{ #category : 'tests - accessing' }
FormSetTest >> testForms [

	| form1 form2 formSet |

	form1 := Form extent: 1@2 depth: 32.
	form2 := Form extent: 2@4 depth: 8.

	formSet := FormSet form: form1.
	self assert: formSet forms equals: { form1 }.

	formSet := FormSet form: form2.
	self assert: formSet forms equals: { form2 }.

	formSet := FormSet forms: { form1. form2 }.
	self assert: formSet forms equals: { form1. form2 }.

	formSet := FormSet forms: { form2. form1 }.
	self assert: formSet forms equals: { form2. form1 }.

	formSet := FormSet extent: 2@4 depth: 8 forms: { form1. form2 }.
	self assert: formSet forms equals: { form1. form2 }.

	formSet := FormSet extent: 3@6 depth: 1 forms: { form2. form1 }.
	self assert: formSet forms equals: { form2. form1 }.
]

{ #category : 'tests - accessing' }
FormSetTest >> testHeight [

	| form1 form2 formSet |

	form1 := Form extent: 1@2 depth: 32.
	form2 := Form extent: 2@4 depth: 8.

	formSet := FormSet form: form1.
	self assert: formSet height equals: 2.

	formSet := FormSet form: form2.
	self assert: formSet height equals: 4.

	formSet := FormSet forms: { form1. form2 }.
	self assert: formSet height equals: 2.

	formSet := FormSet forms: { form2. form1 }.
	self assert: formSet height equals: 4.

	formSet := FormSet extent: 1@2 depth: 32 forms: { form1. form2 }.
	self assert: formSet height equals: 2.

	formSet := FormSet extent: 3@5 depth: 32 forms: { form2. form1 }.
	self assert: formSet height equals: 5.
]

{ #category : 'tests - accessing' }
FormSetTest >> testWidth [

	| form1 form2 formSet |

	form1 := Form extent: 1@2 depth: 32.
	form2 := Form extent: 2@4 depth: 8.

	formSet := FormSet form: form1.
	self assert: formSet width equals: 1.

	formSet := FormSet form: form2.
	self assert: formSet width equals: 2.

	formSet := FormSet forms: { form1. form2 }.
	self assert: formSet width equals: 1.

	formSet := FormSet forms: { form2. form1 }.
	self assert: formSet width equals: 2.

	formSet := FormSet extent: 1@2 depth: 32 forms: { form1. form2 }.
	self assert: formSet width equals: 1.

	formSet := FormSet extent: 3@4 depth: 32 forms: { form2. form1 }.
	self assert: formSet width equals: 3.
]
