running
setUp 
	class := Object subclass: #ClassForTests instanceVariableNames: '' classVariableNames: '' category: 'ClassOrganizer-Tests'.
	organization := ClassOrganization forClass: class.
	organization addCategory: 'empty'.
	organization addCategory: 'one'.
	organization classify: #one under: 'one' suppressIfDefault: true.
	