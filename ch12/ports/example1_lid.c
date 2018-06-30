/* example1_lid.c */

#include <stdio.h>
#include "erl_driver.h"

typedef struct {
  ErlDrvPort port;
} example_data;

static ErlDrvData example_drv_start(ErlDrvPort port, char *buff)
{
  example_data* d = (example_data*)driver_alloc(sizeof(example_data));
  d->port = port;
  return (ErlDrvData)d;
}

static void example_drv_stop(ErlDrvData handle)
{
  driver_free((char*)handle);
}

static void example_drv_output(ErlDrvData handle, char *buff, int bufflen)
{
  example_data* d = (example_data*)handle;
  char fn = buff[0], arg = buff[1], res;
  if (fn == 1) {
    res = twice(arg);
  } else if (fn == 2) {
    res = sum(buff[1], buff[2]);
  }
  driver_output(d->port, &res, 1);
}

ErlDrvEntry example_driver_entry = {
  NULL, /* F_PTR init, N/A */
  example_drv_start,  /* L_PTR start, 포트가 열릴 때 호출됨 */
  example_drv_stop,   /* F_PTR stop, 포트가 닫힐 때 호출됨 */
  example_drv_output, /* F_PTR output, 얼랭이 포트로 데이터를 보냈을 때 호출됨 */
  NULL,               /* F_PTR ready_input, 입력 디스크립터가 읽을 수 있을 때 호출됨 */
  NULL,               /* F_PTR ready_output, 출력 디스크립트가 쓸 수 있을 때 호출됨 */
  "example1_drv" ,    /* char *driver_name, open_port의 인자 */
  NULL, /* F_PTR finish, 언로드될 때 호출됨 */
  NULL, /* F_PTR control, port_command 콜백 */
  NULL, /* F_PTR timeout, 예약됨 */
  NULL, /* F_PTR outputv, 예약됨 */
};

DRIVER_INIT(example_drv) /* driver_entry의 이름과 매치해야 함 */
{
  return &example_driver_entry;
}
