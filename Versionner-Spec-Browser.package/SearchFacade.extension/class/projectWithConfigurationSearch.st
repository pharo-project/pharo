*Versionner-Spec-Browser
projectWithConfigurationSearch
	"Get a  search widget with filled with projects having a ConfigurationOf in the image."

	^ self projectWithConfigurationSearchRejectAll: [ :item | false ]