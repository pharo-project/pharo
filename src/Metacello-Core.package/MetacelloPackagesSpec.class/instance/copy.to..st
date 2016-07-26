actions
copy: specNamed to: spec

	self addMember: 
		(self copyMember 
			name: spec name;
			sourceName: specNamed;
			spec: spec;
			yourself)