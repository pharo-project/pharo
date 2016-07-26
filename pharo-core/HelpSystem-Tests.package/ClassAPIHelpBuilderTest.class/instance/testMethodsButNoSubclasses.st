testing
testMethodsButNoSubclasses
	|topic|
	topic := ClassAPIHelpBuilder 
				buildHierarchicalHelpTopicFrom: Integer 
				withSubclasses: false 
				withMethods: true.
	self assert: topic subtopics size = 2.
	self assert: topic subtopics first title = 'Instance side'.
	self assert: topic subtopics last title = 'Class side'
 

 