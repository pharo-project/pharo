image package loading
packageKeys

	^ self allManagers collect: [ :pck | pck package name asSymbol -> ('*', pck package name asLowercase) ]