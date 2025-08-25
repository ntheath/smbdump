;------------------------------------------------------------------------
;SMBIOS equates.inc
;
;Author:  Nicholas Heath
;
;Copyright 2003, Dell Computer Corp.
;
;Version: 1.0
;------------------------------------------------------------------------
;SMBIOS specific equates
LOW_LIMIT		equ		0f000h
;this is the lower segment address where the anchor string
;for the EPS can be located.
HIGH_LIMIT		equ		0fff0h	
;this is the high limit for the segment address of the anchor string
BYTES_PER_LINE		equ		11h
;Entry point structure equates
ENTRY_CHECKSUM_OFFSET	equ		4h
ENTRY_LENGTH_OFFSET	equ		5h
MAJOR_VER_OFFSET	equ		6h
MINOR_VER_OFFSET	equ		7h
MAX_STRUCT_SIZE_OFFSET	equ		8h
ENTRY_REV_OFFSET	equ		0Ah
INTER_ANCHOR_OFFSET	equ		10h
INTER_CHECKSUM_OFFSET	equ		15h
TABLE_LENGTH_OFFSET	equ		16h
TABLE_ADDRESS_OFFSET	equ		18h
NUM_STRUCTS_OFFSET	equ		1ch
BCD_REV_OFFSET		equ		1eh

;SMBIOS Structure types
BIOS_INFO		equ		0h	;req
SYSTEM_INFO		equ		1h	;req
BASE_BOARD		equ		2h	;optional, req for ePPID
SYSTEM_ENCLOSURE	equ		3h	;req
PROC_INFO		equ		4h	;req, one per proc
CACHE_INFO		equ		7h	;req
PORT_CONNECTOR		equ		8h	;recommended
SYSTEM_SLOTS		equ		9h	;req
ONBOARD_DEVICE_INFO	equ		10h	;optional
OEM_STRINGS		equ		11h	;req
SYSTEM_CONFIG_OPTS	equ		12h	;optional
BIOS_LANGUAGE_INFO	equ		13h	;optional
GROUP_ASSOC		equ		14h	;optional
SYSTEM_EVENT_LOG	equ		15h	;recommended
PHYSICAL_MEM_ARRAY	equ		16h	;req
MEMORY_DEVICE		equ		17h	;required, one per memory-module socket
BIT32_MEM_ERR_INFO	equ		18h	;recommended
MEMORY_ARRAY_MADD	equ		19h	;req
MEMORY_DEVICE_MADD	equ		20h	;req
BUILT_IN_POINT		equ		21h	;req for portables
PORTABLE_BATT		equ		22h	;req for portables
SYSTEM_RESET		equ		23h	;recommended
SYSTEM_PWR_CONTROLS	equ		25h	;recommended
VOLTAGE_PROBE		equ		26h	;req for systems with voltage probes
COOLING_DEVICE		equ		27h	;req for systems with fans, one per fan
TEMP_PROBE		equ		28h	;req for systems with temp probes
ELECT_CURRENT_PROBE	equ		29h	;req for systems with current probes
OOB_REMOTE_ACCESS	equ		30h	;recommended
BIS_EPS			equ		31h	;req if BIS is supported
SYSTEM_BOOT_INFO	equ		32h	;req
BIT64_MEM_ERROR		equ		33h	;req for memory configs above 4GB
MGMNT_DEVICE		equ		34h	;optional
MGMNT_DEVICE_COMP	equ		35h	;optional
MGMNT_DEVICE_THRESH	equ		36h	;optional
MEMORY_CHANNEL		equ		37h	;req or rambus or synclink
REVS_IDS		equ		0D0h	;req
PARALLEL_PORT		equ		0D1h	;req, one per port
SERIAL_PORT		equ		0D2h	;req, one per port
IR_PORT			equ		0D3h	;req, one per port
SYSTEM_DATA_INDIO	equ		0D4h	;req
PROT_VALUE_TYPE1	equ		0D5h	;req
PROT_VALUE_TYPE2	equ		0D6h	;req
NA			equ		0D7h	;as needed
ONBOARD_VID		equ		0D8h	;req
BUILT_IN_KEYB		equ		0D9h	;req for portables
CALLING_INTERFACE	equ		0DAh	;req
DEVICE_BAY		equ		0DBh	;req for device bay systems
PROBE_CUST		equ		0DCh	;req for each probe
COOLING_DEVICE_CUS	equ		0DDh	;req for each cooling device
REMOTE_BIOS_UPDATE	equ		0DEh	;req
MEMORY_DEVICE_SER	equ		0DFh	;req for seralized memory devices

