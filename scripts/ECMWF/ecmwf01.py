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
	    'param'     : "165.128/166.128/167.128",
	    'step'      : "0",
	    'stream'    : "oper",
	    'time'      : "00:00:00/06:00:00/12:00:00/18:00:00",
	    'type'      : "an",
	    'format'    : "netcdf",
	    'target'    : "/home/claudia/kehra/data/Climate/UVT" + str(x) + ".nc"
	})

