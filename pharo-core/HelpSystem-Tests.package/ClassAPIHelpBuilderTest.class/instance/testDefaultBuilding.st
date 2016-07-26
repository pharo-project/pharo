testing
testDefaultBuilding
	|topic|
	topic := ClassAPIHelpBuilder 
				buildHelpTopicFrom: Integer.				 
	self assert: topic subtopics size = 2.
	self assert: topic subtopics first title = 'Instance side'.
	self assert: topic subtopics last title = 'Class side'
 

 