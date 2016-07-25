action
filter
	^ dataSource class
		root:
			(FTRootItem new
				children: (dataSource rootsItems select: [ :each | (pattern matchesIn: (self dataSource toString: each)) notEmpty ]);
				yourself)
		children: dataSource childrenBlock