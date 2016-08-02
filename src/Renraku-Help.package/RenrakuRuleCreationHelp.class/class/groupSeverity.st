pages
groupSeverity

	^ HelpTopic
		title: 'Group and Severity'
		contents: 
'It''s a good idea to assign your rule to a specific group. For this override the #group method and return string with the name of the group. While you can use any name you want, maybe you would like to put your rule into one of the existing groups: ',
(ReRuleManager availableRuleGroups joinUsing: ', '), '.

You can also specify the severity of your rue by returning one of: #information, #warning, or #error symbols from the #severity method.'