private
collectAllShortcuts
	| collection |
	collection := OrderedCollection new.
	KMRepository default categories
		do: [ :cat | 
			cat keymaps
				do: [ :keymapping | 
					collection
						add:
							(KMShortcutDeclaration new
								shortcutName: keymapping name;
								categoryName: cat name;
								yourself) ] ].
	self kmTable items: collection.