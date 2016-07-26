accessing
customDirectory: aFileReference 
	"this is here for preserving backward compatibility with 
	older settings that relied on customDirectory instead
	of cacheDirectory"
	self cacheDirectory: aFileReference