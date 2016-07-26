private
updateDetail
	detailMorph
		ifNil: [^ self].
	detailMorph
		entryDescription: (self selectedEntry descriptionWith: context);
		position: detailPosition menuWidth: self width;
		show