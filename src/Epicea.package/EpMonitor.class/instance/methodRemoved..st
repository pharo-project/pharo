announcement handling
methodRemoved: aMethodRemovedAnnoucement
	"RPackage already unregistered the method so we have to workaround protocol and package."

	self handleAnyErrorDuring: [
		self addEvent: 
			(EpMethodRemoval method: 
				(aMethodRemovedAnnoucement methodAffected asEpiceaRingDefinition
					protocol: aMethodRemovedAnnoucement protocol;
					package: (RPackageOrganizer default 
						packageForProtocol: aMethodRemovedAnnoucement protocol 
						inClass: aMethodRemovedAnnoucement methodAffected methodClass) name;
					yourself))]