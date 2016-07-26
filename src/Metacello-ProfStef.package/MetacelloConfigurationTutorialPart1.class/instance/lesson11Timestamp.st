lessons
lesson11Timestamp
	^Lesson
		title: 'Lesson 11 (Timestamp)'
		lesson: 
'
Smalltalk tools browser fullOnClass: MetacelloTutorialConfig selector: #version11:.

"The timestamp of a version can be defined:"

	(MetacelloTutorialConfig project version: ''1.1'') timestamp.

"When using the OB-Metacello tools the timestamp field is automatically updated to reflect the current
DateAndTime that the update was made.

The timestamp is a String"

ProfStef next.
'