versions
versionv3_1_1: spec
<version: 'v3.1.1'>
spec for: #common do: [
spec blessing: #release;
repository: 'http://smalltalkhub.com/mc/YuriyTymchuk/ScrapYard/main/'.
spec
	package: 'Renraku' with: 'Renraku-YuriyTymchuk.121';
	package: 'Renraku-Test' with: 'Renraku-Test-YuriyTymchuk.21';
	package: 'QualityAssistant' with: 'QualityAssistant-YuriyTymchuk.106';
	package: 'QualityAssistantRecording' with: 'QualityAssistantRecording-YuriyTymchuk.23';
	package: 'QualityAssistant-Test' with: 'QualityAssistant-Test-YuriyTymchuk.2'.
spec postLoadDoIt: #postload ]