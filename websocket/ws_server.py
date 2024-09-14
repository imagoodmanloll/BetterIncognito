import asyncio
import websockets
from exploit.betterincongito import gyat
from colorama import Fore, init

init(autoreset=True)

proto = gyat()

async def handle_connection(websocket, path):
    async for message in websocket:
        if proto.GetStatus() == 0x0:  # Check if injection was successful
            try:
                proto.run_script(message)  # Function to send the script to Roblox
                await websocket.send(Fore.GREEN + "[INFO] Script executed successfully.")
            except Exception as e:
                await websocket.send(Fore.RED + f"[ERROR] Failed to execute script: {e}")
        else:
            await websocket.send(Fore.RED + "[ERROR] Injection failed. Cannot execute script.")

async def main():
    async with websockets.serve(handle_connection, "localhost", 8765):
        await asyncio.Future()  # Run forever

if __name__ == "__main__":
    print(Fore.GREEN + "[INFO] WebSocket server started at ws://localhost:8765")
    asyncio.run(main())
