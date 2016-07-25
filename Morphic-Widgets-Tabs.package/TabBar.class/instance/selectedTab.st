accessing
selectedTab

	  ^selectionHistory ifEmpty: [ nil ] ifNotEmpty: [ selectionHistory last ]