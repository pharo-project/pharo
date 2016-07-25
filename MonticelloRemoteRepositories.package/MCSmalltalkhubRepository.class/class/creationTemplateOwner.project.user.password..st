creation template
creationTemplateOwner: owner project: project user: user password: password
	^ String streamContents: [ :s|
		s 
			nextPutAll: self name; cr;
			tab; nextPutAll: 'owner: '; print: owner; cr;
			tab; nextPutAll: 'project: '; print: project; cr;
			tab; nextPutAll: 'user: '; print: user; cr;
			tab; nextPutAll: 'password: '; print: password ].