dispatching
keymapObserversForCategories: categories

	^categories collect: [ :cat | KMCategoryBinding 
									target: target realTarget 
									morph: target morph 
									category: cat ].