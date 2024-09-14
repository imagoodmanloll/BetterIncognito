import asyncio
import websockets
import logging
from exploit.betterincongito import gyat
from colorama import Fore, init
from websockets.exceptions import ConnectionClosedOK, ConnectionClosedError

init(autoreset=True)

logging.basicConfig(level=logging.INFO, format="%(asctime)s - %(levelname)s - %(message)s")

proto = gyat()

HOST = "localhost"
PORT = 8765

async def handle_connection(websocket, path):
    try:
        async for message in websocket:
            if proto.GetStatus() == 0x0:
                try:
                    proto.run_script(message)
                    await websocket.send(Fore.GREEN + "[INFO] Script executed successfully.")
                except Exception as e:
                    await websocket.send(Fore.RED + f"[ERROR] Failed to execute script: {e}")
            else:
                await websocket.send(Fore.RED + "[ERROR] Injection failed. Cannot execute script.")
    except (ConnectionClosedOK, ConnectionClosedError):
        logging.info(Fore.YELLOW + f"[INFO] Connection closed: {path}")
    except Exception as e:
        logging.error(Fore.RED + f"[ERROR] Unexpected error: {e}")

async def main():
    async with websockets.serve(handle_connection, HOST, PORT):
        await asyncio.Future()

if __name__ == "__main__":
    logging.info(Fore.GREEN + f"[INFO] WebSocket server started at ws://{HOST}:{PORT}")
    asyncio.run(main())
