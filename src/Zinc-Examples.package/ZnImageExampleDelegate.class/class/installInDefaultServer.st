public
installInDefaultServer
	"Assuming the default server has the default delegate, 
	install ourself under /image"
	
	ZnServer default delegate 
		map: 'image' to: self new