#!/usr/bin/env python
import struct
import base64

def b64_encode_array(array,kind):
  data=struct.pack(len(array) * kind, *array)
  return base64.b64encode(data)

if __name__ == '__main__':
  print('Scalars')
  print('R16P, 134.231, base64:'+b64_encode_array([134.231],'d'))
  print('R8P,  1.0,     base64:'+b64_encode_array([1.0    ],'d'))
  print('R4P,  0.0,     base64:'+b64_encode_array([0.0    ],'f'))
  print('I8P,  23,      base64:'+b64_encode_array([23     ],'q'))
  print('I4P,  2023,    base64:'+b64_encode_array([2023   ],'i'))
  print('I2P,  -203,    base64:'+b64_encode_array([-203   ],'h'))
  print('I1P,  +120,    base64:'+b64_encode_array([+120   ],'b'))
  print('Char, hello,   base64:'+base64.b64encode('hello'))
  print('Arrays')
  print('R16P,[121.0,2.32       ], base64:'+b64_encode_array([121.0,2.32       ],'d'))
  print('R8P, [1.0,2.0          ], base64:'+b64_encode_array([1.0,2.0          ],'d'))
  print('R4P, [0.0,-32.12       ], base64:'+b64_encode_array([0.0,-32.12       ],'f'))
  print('I8P, [23,324,25456656,2], base64:'+b64_encode_array([23,324,25456656,2],'q'))
  print('I4P, [2023,-24         ], base64:'+b64_encode_array([2023,-24         ],'i'))
  print('I2P, [-203,-10         ], base64:'+b64_encode_array([-203,-10         ],'h'))
  print('I1P, [+120,-1          ], base64:'+b64_encode_array([+120,-1          ],'b'))
  print('Char,[hello,world      ], base64:'+base64.b64encode(''.join(['hello','world'])))
