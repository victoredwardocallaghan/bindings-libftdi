#include "bindings.dsl.h"
#include "ftdi.h"

{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE ForeignFunctionInterface#-}

-- | <http://libusb.sourceforge.net/api-1.0/group__dev.html>

module Bindings.LibFtdi where

#strict_import

#integral_t enum ftdi_chip_type
#integral_t enum ftdi_parity_type
#integral_t enum ftdi_stopbits_type
#integral_t enum ftdi_bits_type
#integral_t enum ftdi_break_type

#integral_t enum ftdi_mpsse_mode

#integral_t enum ftdi_interface

#integral_t enum ftdi_module_detach_mode

#num MPSSE_WRITE_NEG
#num MPSSE_BITMODE
#num MPSSE_READ_NEG
#num MPSSE_LSB
#num MPSSE_DO_WRITE
#num MPSSE_DO_READ
#num MPSSE_WRITE_TMS

#num SET_BITS_LOW
#num SET_BITS_HIGH
#num GET_BITS_LOW
#num GET_BITS_HIGH
#num LOOPBACK_START
#num LOOPBACK_END
#num TCK_DIVISOR
-- DIV_VALUE(rate) (rate > 6000000)?0:((6000000/rate -1) > 0xffff)? 0xffff: (6000000/rate -1)
divValue rate | rate > 6000000 = 0 
              | ((6000000/rate -1) > 0xffff) = 0xffff
              | otherwise = (6000000/rate -1)

#num SEND_IMMEDIATE
#num WAIT_ON_HIGH
#num WAIT_ON_LOW

#num READ_SHORT
#num READ_EXTENDED
#num WRITE_SHORT
#num WRITE_EXTENDED

#num SIO_RESET
#num SIO_MODEM_CTRL
#num SIO_SET_FLOW_CTRL
#num SIO_SET_BAUD_RATE
#num SIO_SET_DATA

-- #num FTDI_DEVICE_OUT_REQTYPE
-- #num FTDI_DEVICE_IN_REQTYPE

#num SIO_RESET_REQUEST
#num SIO_SET_BAUDRATE_REQUEST
#num SIO_SET_DATA_REQUEST
#num SIO_SET_FLOW_CTRL_REQUEST
#num SIO_SET_MODEM_CTRL_REQUEST
#num SIO_POLL_MODEM_STATUS_REQUEST
#num SIO_SET_EVENT_CHAR_REQUEST
#num SIO_SET_ERROR_CHAR_REQUEST
#num SIO_SET_LATENCY_TIMER_REQUEST
#num SIO_GET_LATENCY_TIMER_REQUEST
#num SIO_SET_BITMODE_REQUEST
#num SIO_READ_PINS_REQUEST
#num SIO_READ_EEPROM_REQUEST
#num SIO_WRITE_EEPROM_REQUEST
#num SIO_ERASE_EEPROM_REQUEST


#num SIO_RESET_SIO
#num SIO_RESET_PURGE_RX
#num SIO_RESET_PURGE_TX

#num SIO_DISABLE_FLOW_CTRL
#num SIO_RTS_CTS_HS
#num SIO_DTR_DSR_HS
#num SIO_XON_XOFF_HS

#num SIO_SET_DTR_MASK
#num SIO_SET_DTR_HIGH
#num SIO_SET_DTR_LOW
#num SIO_SET_RTS_MASK
#num SIO_SET_RTS_HIGH
#num SIO_SET_RTS_LOW

#pointer FTDI_URB_USERCONTEXT_COOKIE

#opaque_t    libusb_device_handle

#starttype struct ftdi_transfer_control
#field    completed, CUInt
#field    buf, Ptr CUChar
#field    size, CUInt
#field    offset, CUInt
#field    ftdi, Ptr <ftdi_context>
-- #field    transfer, Ptr <libusb_transfer>
#stoptype

#starttype struct ftdi_context
-- #field    usb_ctx , Ptr <libusb_context>
#field    usb_dev , Ptr <libusb_device_handle>
#field    usb_read_timeout, CInt
#field    usb_write_timeout, CInt
#field    type , <ftdi_chip_type>
#field    baudrate, CInt
#field    bitbang_enabled , CUChar
#field    readbuffer , Ptr CUChar
#field    readbuffer_offset, CUInt
#field    readbuffer_remaining, CUInt
#field    readbuffer_chunksize, CUInt
#field    writebuffer_chunksize, CUInt
#field    max_packet_size, CUInt
#field    interface, CInt
#field    index, CInt
#field    in_ep, CInt
#field    out_ep, CInt
#field    bitbang_mode , CUChar
-- #field    eeprom, Ptr <ftdi_eeprom>
#field    error_str, Ptr CChar
#field    module_detach_mode, <ftdi_module_detach_mode>
#stoptype

#integral_t enum ftdi_eeprom_value

#opaque_t    libusb_device

#starttype struct ftdi_device_list
#field    next , Ptr <ftdi_device_list>
#field    dev , Ptr <libusb_device>
#stoptype

#num FT1284_CLK_IDLE_STATE
#num FT1284_DATA_LSB
#num FT1284_FLOW_CONTROL
#num POWER_SAVE_DISABLE_H

#num USE_SERIAL_NUM

#integral_t enum ftdi_cbus_func

#num INVERT_TXD
#num INVERT_RXD
#num INVERT_RTS
#num INVERT_CTS
#num INVERT_DTR
#num INVERT_DSR
#num INVERT_DCD
#num INVERT_RI

#num CHANNEL_IS_UART
#num CHANNEL_IS_FIFO
#num CHANNEL_IS_OPTO
#num CHANNEL_IS_CPU
#num CHANNEL_IS_FT1284

#num CHANNEL_IS_RS485

#num DRIVE_4MA
#num DRIVE_8MA
#num DRIVE_12MA
#num DRIVE_16MA
#num SLOW_SLEW
#num IS_SCHMITT

#num DRIVER_VCP
#num DRIVER_VCPH

#num USE_USB_VERSION_BIT

#num SUSPEND_DBUS7_BIT

#num HIGH_CURRENT_DRIVE
#num HIGH_CURRENT_DRIVE_R

-- XXX

#starttype struct ftdi_version_info
#field    major, CInt
#field    minor, CInt
#field    micro, CInt
#field    version_str, Ptr CChar
#field    snapshot_str, Ptr CChar
#stoptype

-- #starttype struct ftdi_eeprom
-- #field    vendor_id , CInt
-- #field    product_id , CInt
-- #field    self_powered , CInt
-- #field    remote_wakeup , CInt
-- #field    BM_type_chip , CInt
-- #field    in_is_isochronous , CInt
-- #field    out_is_isochronous , CInt
-- #field    suspend_pull_downs , CInt
-- #field    use_serial , CInt
-- #field    change_usb_version , CInt
-- #field    usb_version , CInt
-- #field    max_power , CInt
-- #field    manufacturer , Ptr CChar
-- #field    product , Ptr CChar
-- #field    serial , Ptr CChar
-- #field    size , CInt
-- #stoptype

#ccall    ftdi_init , Ptr <ftdi_context> -> IO CInt
#ccall    ftdi_new , IO ( Ptr <ftdi_context> )
#ccall    ftdi_set_interface , Ptr <ftdi_context> -> <ftdi_interface> -> IO CInt

#ccall    ftdi_deinit , Ptr <ftdi_context> -> IO ()
#ccall    ftdi_free , Ptr <ftdi_context> -> IO ()
#ccall    ftdi_set_usbdev , Ptr <ftdi_context> -> Ptr <libusb_device_handle> -> IO ()

-- #ccall    ftdi_get_library_version, IO <ftdi_version_info>

#ccall    ftdi_usb_find_all , Ptr <ftdi_context> -> Ptr (Ptr <ftdi_device_list>) -> CInt -> CInt -> IO CInt
#ccall    ftdi_list_free , Ptr (Ptr <ftdi_device_list>) -> IO ()
#ccall    ftdi_list_free2 , Ptr <ftdi_device_list> -> IO ()
#ccall    ftdi_usb_get_strings , Ptr <ftdi_context> -> Ptr <libusb_device> -> Ptr CChar -> CInt -> Ptr CChar -> CInt -> Ptr CChar -> CInt -> IO CInt
#ccall    ftdi_eeprom_set_strings , Ptr <ftdi_context> -> Ptr CChar -> Ptr CChar -> Ptr CChar -> IO CInt

#ccall    ftdi_usb_open , Ptr <ftdi_context> -> CInt -> CInt -> IO CInt
#ccall    ftdi_usb_open_desc , Ptr <ftdi_context> -> CInt -> CInt -> Ptr CChar -> Ptr CChar -> IO CInt
#ccall    ftdi_usb_open_dev , Ptr <ftdi_context> -> Ptr <libusb_device> -> IO CInt
#ccall    ftdi_usb_open_string , Ptr <ftdi_context> -> CString -> IO CInt

#ccall    ftdi_usb_close , Ptr <ftdi_context> -> IO CInt
#ccall    ftdi_usb_reset , Ptr <ftdi_context> -> IO CInt
#ccall    ftdi_usb_purge_rx_buffer , Ptr <ftdi_context> -> IO CInt
#ccall    ftdi_usb_purge_tx_buffer , Ptr <ftdi_context> -> IO CInt
#ccall    ftdi_usb_purge_buffers , Ptr <ftdi_context> -> IO CInt

#ccall    ftdi_set_baudrate , Ptr <ftdi_context> -> CInt -> IO CInt
#ccall    ftdi_set_line_property , Ptr <ftdi_context> -> <ftdi_bits_type> -> <ftdi_stopbits_type> -> <ftdi_parity_type> -> IO CInt
#ccall    ftdi_set_line_property2 , Ptr <ftdi_context> -> <ftdi_bits_type> -> <ftdi_stopbits_type> -> <ftdi_parity_type> -> <ftdi_break_type> -> IO CInt

#ccall    ftdi_read_data , Ptr <ftdi_context> -> Ptr CUChar -> CInt -> IO CInt
#ccall    ftdi_read_data_set_chunksize , Ptr <ftdi_context> -> CUInt -> IO CInt
#ccall    ftdi_read_data_get_chunksize , Ptr <ftdi_context> -> Ptr CUInt -> IO CInt

#ccall    ftdi_write_data , Ptr <ftdi_context> -> Ptr CUChar -> CUInt -> IO CInt
#ccall    ftdi_write_data_set_chunksize , Ptr <ftdi_context> -> CUInt -> IO CInt
#ccall    ftdi_write_data_get_chunksize , Ptr <ftdi_context> -> Ptr CUInt -> IO CInt

#ccall    ftdi_set_bitmode , Ptr <ftdi_context> -> CUChar -> CUChar -> IO CInt
#ccall    ftdi_disable_bitbang , Ptr <ftdi_context> -> IO CInt
#ccall    ftdi_read_pins , Ptr <ftdi_context> -> Ptr CUChar -> IO CInt

#ccall    ftdi_set_latency_timer , Ptr <ftdi_context> -> CUChar -> IO CInt
#ccall    ftdi_get_latency_timer , Ptr <ftdi_context> -> Ptr CUChar -> IO CInt

#ccall    ftdi_poll_modem_status , Ptr <ftdi_context> -> Ptr CShort -> IO CInt

#ccall    ftdi_setflowctrl , Ptr <ftdi_context> -> CInt -> IO CInt
#ccall    ftdi_setdtr_rts , Ptr <ftdi_context> -> CInt -> CInt -> IO CInt
#ccall    ftdi_setdtr , Ptr <ftdi_context> -> CInt -> IO CInt
#ccall    ftdi_setrts , Ptr <ftdi_context> -> CInt -> IO CInt

#ccall    ftdi_set_event_char , Ptr <ftdi_context> -> CUChar -> CUChar -> IO CInt
#ccall    ftdi_set_error_char , Ptr <ftdi_context> -> CUChar -> CUChar -> IO CInt

#ccall    ftdi_eeprom_initdefaults , Ptr <ftdi_context> -> Ptr CChar -> Ptr CChar -> Ptr CChar -> IO CInt
#ccall    ftdi_eeprom_build , Ptr <ftdi_context> -> IO CInt
#ccall    ftdi_eeprom_decode , Ptr <ftdi_context> -> CInt -> IO CInt

#ccall    ftdi_get_eeprom_value, Ptr <ftdi_context> -> <ftdi_eeprom_value> -> Ptr CInt -> IO CInt
#ccall    ftdi_set_eeprom_value, Ptr <ftdi_context> -> <ftdi_eeprom_value> -> CInt -> IO CInt

#ccall    ftdi_get_eeprom_buf, Ptr <ftdi_context> -> Ptr CUChar -> CInt -> IO CInt
#ccall    ftdi_set_eeprom_buf, Ptr <ftdi_context> -> Ptr CUChar -> CInt -> IO CInt

#ccall    ftdi_read_eeprom , Ptr <ftdi_context> -> IO CInt
#ccall    ftdi_read_chipid , Ptr <ftdi_context> -> Ptr CUInt -> IO CInt
#ccall    ftdi_write_eeprom , Ptr <ftdi_context> -> IO CInt
#ccall    ftdi_erase_eeprom , Ptr <ftdi_context> -> IO CInt

#ccall    ftdi_read_eeprom_location  , Ptr <ftdi_context> -> CInt -> Ptr CUShort -> IO CInt
#ccall    ftdi_write_eeprom_location , Ptr <ftdi_context> -> CInt -> CUShort -> IO CInt

#ccall    ftdi_get_error_string , Ptr <ftdi_context> -> IO (Ptr CChar)
