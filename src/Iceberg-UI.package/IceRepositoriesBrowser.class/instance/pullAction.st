menu actions - selected repository
pullAction
	^ GLMGenericAction new
		selectionAction: #pull;
		selectionCondition: #isOperative;
		icon: GLMUIThemeExtraIcons glamorousLeftSide;
		showTitle: 'Pull incoming commits'; 
		shortcut: $p; 
		yourself
