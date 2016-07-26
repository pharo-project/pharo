accessing
removeProperty: aSymbol
	properties := properties copyWithout: (Association
											key: aSymbol
											value: (properties propertyValueAt: aSymbol))