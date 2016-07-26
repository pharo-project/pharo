submorphs-add/remove
dismissViaHalo

	super dismissViaHalo.
	selectedItems do: [:m | m dismissViaHalo]