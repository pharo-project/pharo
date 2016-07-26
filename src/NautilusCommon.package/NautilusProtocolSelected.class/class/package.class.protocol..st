instance creation
package: aPackage class: aClass protocol: aProtocol

	^ self new
		selectedPackage: aPackage;
		selectedClass: aClass;
		selectedProtocol: aProtocol;
		yourself