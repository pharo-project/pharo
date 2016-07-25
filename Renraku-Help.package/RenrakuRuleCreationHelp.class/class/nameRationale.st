pages
nameRationale

	^ HelpTopic
		title: 'Name and Rationale'
		contents: 
'Each rule should provide a short name string returned from the #name method. You also have to override the #rationale method to return a detailed description about the rule. You may also put the rationale in the class comment, as by default #rationale method returns the comment of the rule''s class.'