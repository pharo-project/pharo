"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestWebBrowserCore,
	#superclass : #PackageManifest,
	#category : #'WebBrowser-Core-Manifest'
}

{ #category : #'code-critics' }
ManifestWebBrowserCore class >> ruleClassNotReferencedRuleV1FalsePositive [
	^ #(#(#(#RGClassDefinition #(#WBMacWebBrowser)) #'2020-11-25T00:28:23.039864+01:00') #(#(#RGClassDefinition #(#WBUnixWebBrowser)) #'2020-11-25T00:28:31.340856+01:00') #(#(#RGClassDefinition #(#WBWindowsWebBrowser)) #'2020-11-25T00:28:37.162633+01:00') )
]
