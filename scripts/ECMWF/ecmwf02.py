#!/usr/bin/env python
from ecmwfapi import ECMWFDataServer

server = ECMWFDataServer()

for x in range(1981, 2015):
	
	server.retrieve({
	    'class'     : "ei",
	    'dataset'   : "interim",
	    'date'      : str(x) + "-01-01/to/" + str(x) + "-12-31",
	    'expver'    : "1",
	    'grid'      : "0.75/0.75", 
	    'levtype'   : "sfc",
	    'param'     : "159.128/176.128/228.128",
	    'step'      : "12",
	    'stream'    : "oper",
	    'time'      : "00:00:00/12:00:00",
	    'type'      : "fc",
	    'format'    : "netcdf",
	    'target'    : "/home/claudia/kehra/data/Climate/PBR" + str(x) + ".nc"
	})

