#include "bindings.dsl.h"
#include "ftdi.h"

{-# LANGUAGE ForeignFunctionInterface#-}
-- | <http://libusb.sourceforge.net/api-1.0/group__dev.html>

module Bindings.Libftdi where
#strict_import

#num FTDI_DEFAULT_EEPROM_SIZE 

#integral_t enum ftdi_chip_type
#integral_t enum ftdi_parity_type
#integral_t enum ftdi_stopbits_type
#integral_t enum ftdi_bits_type
#integral_t enum ftdi_break_type

#integral_t enum ftdi_mpsse_mode

#integral_t enum ftdi_interface

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

#num FTDI_DEVICE_OUT_REQTYPE
#num FTDI_DEVICE_IN_REQTYPE

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

#opaque_t    usb_dev_handle

#starttype struct ftdi_context
#field    usb_dev , Ptr <usb_dev_handle>
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
#field    in_ep, CInt
#field    bitbang_mode , CUChar 
#field    eeprom_size, CInt
#field    error_str, Ptr CChar
#field    async_usb_buffer , Ptr CChar
#field    async_usb_buffer_size, CUInt
#stoptype 

#opaque_t    usb_device

#starttype struct ftdi_device_list
#field    next , Ptr <ftdi_device_list>
#field    dev , Ptr <usb_device>
#stoptype

#starttype struct ftdi_eeprom
#field    vendor_id , CInt
#field    product_id , CInt
#field    self_powered , CInt
#field    remote_wakeup , CInt
#field    BM_type_chip , CInt
#field    in_is_isochronous , CInt
#field    out_is_isochronous , CInt
#field    suspend_pull_downs , CInt
#field    use_serial , CInt
#field    change_usb_version , CInt
#field    usb_version , CInt
#field    max_power , CInt
#field    manufacturer , Ptr CChar
#field    product , Ptr CChar
#field    serial , Ptr CChar
#field    size , CInt
#stoptype

#ccall    ftdi_init , Ptr <ftdi_context> -> IO CInt
#ccall    ftdi_new , IO ( Ptr <ftdi_context> )
#ccall    ftdi_set_interface , Ptr <ftdi_context> -> <ftdi_interface> -> IO CInt
#ccall    ftdi_deinit , Ptr <ftdi_context> -> IO ()
#ccall    ftdi_free , Ptr <ftdi_context> -> IO ()
#ccall    ftdi_set_usbdev , Ptr <ftdi_context> -> Ptr <usb_dev_handle> -> IO ()
#ccall    ftdi_usb_find_all , Ptr <ftdi_context> -> Ptr (Ptr <ftdi_device_list>) -> CInt -> CInt -> IO CInt
#ccall    ftdi_list_free , Ptr (Ptr <ftdi_device_list>) -> IO ()
#ccall    ftdi_list_free2 , Ptr <ftdi_device_list> -> IO ()
#ccall    ftdi_usb_get_strings , Ptr <ftdi_context> -> Ptr <usb_device> -> Ptr CChar -> CInt -> Ptr CChar -> CInt -> Ptr CChar -> CInt -> IO CInt
#ccall    ftdi_usb_open , Ptr <ftdi_context> -> CInt -> CInt -> IO CInt
#ccall    ftdi_usb_open_desc , Ptr <ftdi_context> -> CInt -> CInt -> Ptr CChar -> Ptr CChar -> IO CInt
#ccall    ftdi_usb_open_dev , Ptr <ftdi_context> -> Ptr <usb_device> -> IO CInt
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
#ccall    ftdi_write_data_async , Ptr <ftdi_context> -> Ptr CUChar -> CInt -> IO CInt
#ccall    ftdi_async_complete , Ptr <ftdi_context> -> CInt -> IO ()
#ccall    ftdi_enable_bitbang , Ptr <ftdi_context> -> CUChar -> IO CInt
#ccall    ftdi_disable_bitbang , Ptr <ftdi_context> -> IO CInt
#ccall    ftdi_set_bitmode , Ptr <ftdi_context> -> CUChar -> CUChar -> IO CInt
#ccall    ftdi_read_pins , Ptr <ftdi_context> -> CUChar -> IO CInt
#ccall    ftdi_set_latency_timer , Ptr <ftdi_context> -> CUChar -> IO CInt
#ccall    ftdi_get_latency_timer , Ptr <ftdi_context> -> Ptr CUChar -> IO CInt
#ccall    ftdi_poll_modem_status , Ptr <ftdi_context> -> Ptr CShort -> IO CInt
#ccall    ftdi_setflowctrl , Ptr <ftdi_context> -> CInt -> IO CInt
#ccall    ftdi_setdtr_rts , Ptr <ftdi_context> -> CInt -> CInt -> IO CInt
#ccall    ftdi_setdtr , Ptr <ftdi_context> -> CInt -> IO CInt
#ccall    ftdi_setrts , Ptr <ftdi_context> -> CInt -> IO CInt
#ccall    ftdi_set_event_char , Ptr <ftdi_context> -> CUChar -> CUChar -> IO CInt
#ccall    ftdi_set_error_char , Ptr <ftdi_context> -> CUChar -> CUChar -> IO CInt
#ccall    ftdi_eeprom_setsize , Ptr <ftdi_context> -> Ptr <ftdi_eeprom> -> CInt -> IO ()
#ccall    ftdi_eeprom_initdefaults , Ptr <ftdi_eeprom> -> IO ()
#ccall    ftdi_eeprom_build , Ptr <ftdi_eeprom> -> Ptr CUChar -> IO CInt
#ccall    ftdi_eeprom_decode , Ptr <ftdi_eeprom> -> Ptr CUChar -> CInt -> IO CInt
#ccall    ftdi_read_eeprom , Ptr <ftdi_context> -> Ptr CUChar -> IO CInt
#ccall    ftdi_read_chipid , Ptr <ftdi_context> -> Ptr CUInt -> IO CInt
#ccall    ftdi_read_eeprom_getsize , Ptr <ftdi_context> -> Ptr CUChar -> CInt -> IO CInt
#ccall    ftdi_write_eeprom , Ptr <ftdi_context> -> Ptr CUChar -> IO CInt
#ccall    ftdi_erase_eeprom , Ptr <ftdi_context> -> IO CInt
#ccall    ftdi_get_error_string , Ptr <ftdi_context> -> IO (Ptr CChar)
