#!/usr/bin/env python
import struct
print struct.pack('d', 1.0).encode('base64')
print struct.pack('f', 0.0).encode('base64')
print struct.pack('q', 23).encode('base64')
print struct.pack('i', 2023).encode('base64')
print struct.pack('h', -203).encode('base64')
print struct.pack('b', +120).encode('base64')
